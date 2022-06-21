#!/usr/bin/env node

const fs = require('fs')
const readYaml = require('node-read-yaml')

const indexHtml = `${__dirname}/${process.argv[2]}/index.html`
const tipsDir = `${__dirname}/public/tips`

const tips = fs
  .readdirSync(tipsDir)
  .sort((a, b) => Number(a.split('-')[0]) - Number(b.split('-')[0]))
  .map(filename => JSON.stringify(readYaml.sync(`${tipsDir}/${filename}`)))
  .join(', ')

fs.readFile(indexHtml, 'utf8', (err, data) => {
  if (err) { return console.log(err) }

  var html = data.replace(/const\sTIPS.*/g, 'const TIPS = [' + tips + ']')

  fs.writeFile(indexHtml, html, 'utf8', (err) => {
     if (err) { return console.log(err) }
  })
})
