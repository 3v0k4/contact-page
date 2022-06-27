#!/usr/bin/env node

const fs = require('fs')
const readYaml = require('node-read-yaml')
const hljs = require('highlight.js')

const indexHtml = `${__dirname}/${process.argv[2]}/index.html`
const tipsDir = `${__dirname}/public/tips`

const toHtml = ({ index, title, description, badCode, goodCode }) => `
  <div data-index="${index}" data-title="${title.replace(/\s/g, '-')}">
    <div style="text-align: center; margin-top: 20px;">
      <a style="color: rgb(156, 163, 175);" href="https://typescript.tips/?index=${index}#tipTitle">View this tip in your browser</a>
    </div>

    <div style="margin-top: 20px; text-align: center;">
      <img style="width: 100px; height: 100px; margin: auto;" src="https://typescript.tips/logo.png" alt="blue square containing tips in yellow and ts in white" />
    </div>

    <br />

    <div style="text-align: center;">
      <h1 style="font-size: 2rem;">${title}</h1>
      <div style="line-height: 1rem;">
        <div style="margin-top: 10px;">
          <a style="color: rgb(156, 163, 175); font-size: 0.8rem;" href="https://twitter.com/RiccardoOdone">@RiccardoOdone</a>
        </div>
        <div>
          <span style="color: rgb(156, 163, 175); font-size: 0.8rem;">•</span>
        </div>
        <div>
          <a style="color: rgb(156, 163, 175);  font-size: 0.8rem;" href="https://twitter.com/RiccardoOdone">Suggest a tip</a>
        </div>
      </div>

      <br />
      <br />

      <span style="font-size: 1.1rem;">${description}</span>

      <br />
      <br />
      <br />

      <div>
        <img  style="width: 61px; height: 64px; margin: auto;" src="http://typescript.tips/x.png" alt="red circle containing a white x" />
        <div style="padding: 10px 0; text-align: left; border-radius: 5px;">
          <div style="background-color: rgb(243, 244, 246); filter: drop-shadow(rgba(0, 0, 0, 0.04) 0px 10px 8px) drop-shadow(rgba(0, 0, 0, 0.1) 0px 4px 3px); border-radius: 5px;">
            <span style="margin-top: 12px; border-radius: 50%; width: 13px; height: 13px; display: inline-block; margin-left: 20px; background-color: rgb(209, 213, 219);"></span>
            <span style="margin-top: 12px; border-radius: 50%; width: 13px; height: 13px; display: inline-block; margin-left: 8px; background-color: rgb(209, 213, 219);"></span>
            <span style="margin-top: 12px; border-radius: 50%; width: 13px; height: 13px; display: inline-block; margin-left: 8px; background-color: rgb(209, 213, 219);"></span>
            <pre style="background-color: rgb(243, 244, 246); padding: 0 20px 20px 20px; font-size: 0.9rem;"><code>${hljs.highlight(badCode || '', {language: 'typescript'}).value}</code></pre>
          </div>
        </div>
      </div>

      <br />

      <div>
        <img  style="width: 61px; height: 64px; margin: auto;" src="http://typescript.tips/v.png" alt="green circle containing a white v" />

        <div style="padding: 10px 0; text-align: left; border-radius: 5px;">
          <div style="background-color: rgb(243, 244, 246); filter: drop-shadow(rgba(0, 0, 0, 0.04) 0px 10px 8px) drop-shadow(rgba(0, 0, 0, 0.1) 0px 4px 3px); border-radius: 5px;">
            <span style="margin-top: 12px; border-radius: 50%; width: 13px; height: 13px; display: inline-block; margin-left: 20px; background-color: rgb(209, 213, 219);"></span>
            <span style="margin-top: 12px; border-radius: 50%; width: 13px; height: 13px; display: inline-block; margin-left: 8px; background-color: rgb(209, 213, 219);"></span>
            <span style="margin-top: 12px; border-radius: 50%; width: 13px; height: 13px; display: inline-block; margin-left: 8px; background-color: rgb(209, 213, 219);"></span>

            <pre style="background-color: rgb(243, 244, 246); padding: 0 20px 20px 20px; font-size: 0.9rem;"><code>${hljs.highlight(goodCode || '', {language: 'typescript'}).value}</code></pre>
          </div>
        </div>
      </div>
    </div>
  </div>
`

const layout = html => `
  <html lang="en">
    <head>
      <meta charset="utf-8">
      <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.1/styles/mono-blue.min.css" crossorigin="anonymous">
    </head>

    <body>
      ${html}
    </body>

    <script>
      Array.prototype.unique = function(a){
        return function(){return this.filter(a)}
      }(function(a,b,c){return c.indexOf(a,b+1)<0})

      // get CSS rules from style tag
      var cssrules = document.styleSheets[0].cssRules

      for (var i=0; i<cssrules.length; i++){
        var rule = cssrules[i]

        var items = document.querySelectorAll(rule.selectorText)

        for (var j=0; j<items.length; j++){
          // add to previous styles
          var style = items[j].getAttribute('style') || ''
          style = style + ';' + rule.style.cssText
          // clean and remove duplicate styles
          style = style.replace(/\s*(:|;|,|\(|\))\s*/g, '$1')
          style = style.replace(/\s+/g, ' ')
          style = style.split(';').unique().join(';')
          // apply to tag
          items[j].setAttribute('style',  style)

          // console.log('style applied', items[j])
        }
      }
    </script>
  </html>
`

const html = layout(
  fs
  .readdirSync(tipsDir)
  .sort((a, b) => Number(a.split('-')[0]) - Number(b.split('-')[0]))
  .map(filename => [filename, readYaml.sync(`${tipsDir}/${filename}`)])
  .map(([filename, props]) => toHtml({ ...props, index: filename.split('-')[0] }))
  .join('')
)

fs.writeFile('./emails.html', html, 'utf8', (err) => {
  if (err) { return console.log(err) }
})

const exec = require('child_process').exec
exec('open ./emails.html', function(err) {
  if (err) { return console.log(err) }

  setTimeout(() => {
    exec('rm ./emails.html', function(err) {
      if (err) { return console.log(err) }
    })
  }, 1000)
})
