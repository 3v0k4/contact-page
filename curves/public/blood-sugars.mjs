import { debounce } from './debounce.mjs';
import 'https://unpkg.com/konva@8/konva.min.js';
import { html, define } from 'https://unpkg.com/hybrids@^6';

const getControlY = (curve) => (curve.ym - 2 * Math.pow(0.5, 2) * curve.y) / (2 * Math.pow(0.5, 2))

const drawQuadratic = ({ layer, y, c, side }) => {
  const curve = new Konva.Shape({
    stroke: 'black',
    strokeWidth: 1,
    listening: false,
    sceneFunc: (ctx, shape) => {
      ctx.beginPath();
      ctx.moveTo(c.x()-side(), y);
      const cy = getControlY({ y, ym: c.y() })
      ctx.quadraticCurveTo(c.x(), cy, c.x()+side(), y);
      ctx.fillStrokeShape(shape);
    },
  });
  layer.add(curve);
  return { y, c, side };
};

const drawPoint = ({ layer, x, y, color, size, id, draggable = false }) => {
  const circle = new Konva.Circle({
    x,
    y,
    radius: size,
    fill: color || '#ddd',
    stroke: color || '#666',
    strokeWidth: 2,
    draggable,
    listening: draggable,
    id,
  });

  layer.add(circle);
  return circle;
};

const derivative = (t, curve) =>
  0.5 * (1 - t) * (curve.y - curve.c.y()) + 0.5 * t * (curve.c.y() - curve.y);

const derivativeAt = (x, curve) => {
  const x1 = curve.c.x() - curve.side();
  const x2 = curve.c.x() + curve.side();
  const t = (x - x1) / (x2 - x1);
  return derivative(t, curve) - derivative(0, curve);
}

const setupStage = (root) => {
  const stage = new Konva.Stage({
    container: root.querySelector('#canvaz'),
    width: 800,
    height: 400,
  });
  const layer = new Konva.Layer();
  stage.add(layer);
  return stage;
};

const drawSum = (curves, horizon, layer) => {
  layer.find('#blood-sugar').forEach(x => x.destroy());

  const xs = Array(1000).fill().map((_, i) => i);

  curves = curves.map(c => ({ ...c, c: { x: () => c.c.x(), y: () => getControlY({ y: horizon, ym: c.c.y() })}}))

  const segments = curves
    .map(curve => [
      { x1: 0, x2: curve.c.x()-curve.side(), f: () => 0 },
      { x1: curve.c.x()-curve.side(), x2: curve.c.x()+curve.side(), f: x => derivativeAt(x, curve) },
      { x1: curve.c.x()+curve.side(), x2: xs.length-1, f: () => 0 },
    ])
    .flat();

  const subSegments = [...new Set(segments.map(f => [f.x1, f.x2]).flat())]
    .sort((a, b) => a - b)
    .reduce((acc, x) => x === 0 ? [[0, 0]] : [...acc, [acc[acc.length-1][1], x]], [])
    .slice(1)
    .map(([x1, x2]) => {
      const currentSegments = segments.filter(s => x1 >= s.x1 && x2 <= s.x2)
      return {
        x1: Math.max(...currentSegments.map(s => s.x1)),
        x2: Math.min(...currentSegments.map(s => s.x2)),
        f: x => currentSegments.map(s => s.f(x)).reduce((acc, x) => acc + x, 0),
      }
    })

  const alignedSegments = subSegments.reduce((acc, s) => [
    ...acc,
    {
      ...s,
      f: x => {
        const previousSegment = acc[acc.length-1];
        if (!previousSegment) return s.f(x);
        const lastYPreviousSegment = previousSegment.f(previousSegment.x2);
        const firstYCurrentSegment = s.f(s.x1);
        const gap = lastYPreviousSegment - firstYCurrentSegment;
        return s.f(x) + gap;
      }
    }], []);

  xs
    .filter(i => i % 5 === 0)
    .map(x => ({ x, y: alignedSegments.find(s => x >= s.x1 && x <= s.x2).f(x) }))
    .forEach(({ x, y }) => { drawPoint({ layer, x, y: y + horizon, color: '#BF0000', size: 1, id: 'blood-sugar' }) });
};

const setupRange = (curvez, curves, horizon, root, layer, update) => {
  let timeout;

  let input = root.getElementById('range');

  input.addEventListener('input', function(event) {
    clearTimeout(timeout);
    timeout = setTimeout(() => input.style.display = 'none', 2000);
    const value = parseInt(event.target.value, 10);
    curvez.find(x => x.id === input.dataset.anchor).side = value
    layer.draw();
    update(curves, horizon, layer);
  })

  return {
    clearTimeout: () => clearTimeout(timeout),
    hide: () => {
      clearTimeout(timeout);
      timeout = setTimeout(() => input.style.display = 'none', 2000);
    },
    input,
    showFor: (curve, c) => {
      input.style.top = `${c.y() - 50}px`;
      input.style.left = `${c.x() - 50}px`;
      clearTimeout(timeout);
      input.style.display = 'block';
      input.value = curvez.find(x => x.id === curve).side;
      input.dataset.anchor = curve;
    },
  };
}

const setupControlPointFor = (c, curve, range, horizon, curves, update, layer, stage) => {
  c.on('dragmove', () => {
    update(curves, horizon, layer);
    c.y(Math.min(Math.max(c.y(), 0), stage.height()));
    c.x(Math.min(Math.max(c.x(), 0), stage.width()));
    range.showFor(curve, c);
  });

  c.on('mouseover', function() {
    document.body.style.cursor = 'pointer';
    this.strokeWidth(4);
    range.showFor(curve, c);
  });

  c.on('touchstart', function() {
    this.strokeWidth(4);
    range.showFor(curve, c);
  });

  c.on('mouseout', function() {
    document.body.style.cursor = 'default';
    this.strokeWidth(2);
    range.hide();
  });

  c.on('touchend', function() {
    this.strokeWidth(2);
    range.hide();
  });
};

const draw = (curvez, update, stage, root) => {
  const horizon = stage.height() / 2;
  const layer = stage.getLayers()[0];
  let curves = [];
  const range = setupRange(curvez, curves, horizon, root, layer, update);
  curvez.forEach(ccc => {
    const c = drawPoint({ layer, x: ccc.x, y: horizon+ccc.hh, size: 15, draggable: true });
    setupControlPointFor(c, ccc.id, range, horizon, curves, update, layer, stage);
    curves.push(drawQuadratic({ layer, y: horizon, c, side: () => ccc.side }));
  })
  const mid = new Konva.Line({
    points: [0, horizon, stage.width(), horizon],
    stroke: 'black',
    strokeWidth: 0.2,
    lineCap: 'round',
    lineJoin: 'round',
    dash: [5, 10],
  });
  layer.add(mid);
  const high = new Konva.Line({
    points: [0, horizon-50, stage.width(), horizon-50],
    stroke: 'red',
    strokeWidth: 0.2,
    lineCap: 'round',
    lineJoin: 'round',
  });
  layer.add(high);
  const highLabel = new Konva.Text({
    x: 0,
    y: horizon-50,
    text: 'High',
    fontSize: 10,
    fontFamily: 'sans-serif',
    fill: 'red',
  });
  layer.add(highLabel);
  const low = new Konva.Line({
    points: [0, horizon+50, stage.width(), horizon+50],
    stroke: 'blue',
    strokeWidth: 0.2,
    lineCap: 'round',
    lineJoin: 'round',
  });
  layer.add(low);
  const lowLabel = new Konva.Text({
    x: 0,
    y: horizon+50,
    text: 'Low',
    fontSize: 10,
    fontFamily: 'sans-serif',
    fill: 'blue',
  });
  layer.add(lowLabel);
  update(curves, horizon, layer);
}

const fitStageIntoParentContainer = (root, stage) => {
  const container = root.querySelector('#canvas-container');
  const containerWidth = container.offsetWidth;
  const sceneWidth = parseInt(stage.width(), 10);
  const scale = containerWidth / sceneWidth;
  stage.width(sceneWidth * scale);
}

const setup = ({ root, curvez }) => {
  const stage = setupStage(root);
  const drawSumDebounced = debounce(drawSum, 300);
  draw(curvez, drawSumDebounced, stage, root);
  fitStageIntoParentContainer(root, stage);
  window.addEventListener('resize', () => fitStageIntoParentContainer(root, stage));
}

define({
  tag: "blood-sugars",
  h1: '',
  h2: '',
  x1: '',
  x2: '',
  side1: '',
  side2: '',
  canvas: {
    get: (host) => {
      let curvez = [];
      if (parseInt(host.h1, 10) && parseInt(host.x1, 10) && parseInt(host.side1, 10)) {
        curvez.push({ x: parseInt(host.x1, 10), hh: -parseInt(host.h1, 10), side: parseInt(host.side1, 10), id: 'curve1' });

      }
      if (parseInt(host.h2, 10) && parseInt(host.x2, 10) && parseInt(host.side2, 10)) {
        curvez.push({ x: parseInt(host.x2, 10), hh: parseInt(host.h2, 10), side: parseInt(host.side2, 10), id: 'curve2' });
      }
      const opts = {
        root: host.shadowRoot,
        curvez,
      }
      setup(opts)
      return host;
    },
    observe() {}
  },
  render: () => html`
    <div id="canvas-container" style="position: relative;">
      <div id="canvaz" style="z-index: 1;"></div>
      <input type="range" id="range" min="0" max="300" value="100" data-anchor="" style="display: none; position: absolute; z-index: 2; top: 10px; left: 10px;"></input>
    </div>
  `
});
