import { DOMParser } from 'xmldom';
import * as xpath1 from 'xpath-ts';
import * as xpath2 from 'xpath2';

const input = document.getElementById('input') as HTMLInputElement;
const textarea = document.getElementById('textarea') as HTMLTextAreaElement;
const resultContainer = document.getElementById('result');

if (!resultContainer) throw new Error('cannot find #result');

const highlight = () => {
  const xml = textarea.value;
  var doc = new DOMParser().parseFromString(xml);
  const xpath = input.value;
  const nodes = xpath2.select(xpath, doc);

  if (Array.isArray(nodes)) {
    nodes.forEach(toWrap => {
      const wrapper = doc.createElement('xpath-match');
      toWrap.parentNode?.insertBefore(wrapper, toWrap);
      wrapper.appendChild(toWrap);
    })
  }

  const escape = document.createElement('textarea');
  escape.textContent = doc.toString();

  const result = escape
    .innerHTML
    .replace(/&lt;xpath-match&gt;/mg, '<span class="highlight">')
    .replace(/&lt;\/xpath-match&gt;/mg, '</span>');

  resultContainer.innerHTML = result;
}

input.addEventListener('input', highlight);
textarea.addEventListener('input', highlight);
highlight();
