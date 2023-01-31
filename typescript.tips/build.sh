#!/usr/bin/env node

const fs = require('fs')
const readYaml = require('node-read-yaml')

const templateHtml = `${__dirname}/dist/template.html`
const tipsDir = `${__dirname}/public/tips`

const path = (filename) =>
  filename.split('-').slice(1).join('-').split('.')[0]

const dict = {
  title: ({ tip }) => `${tip.title} | TypeScript Tips`,
  description: ({ tip }) => tip.description,
  canonical: ({ filename }) => {
    return `https://typescript.tips/${encodeURIComponent(path(filename))}/`
  },
  version: () => new Date().getTime()
}

const tips = fs
  .readdirSync(tipsDir)
  .sort((a, b) => Number(a.split('-')[0]) - Number(b.split('-')[0]))
  .map(filename => [filename, readYaml.sync(`${tipsDir}/${filename}`)])
  .map(([filename, props]) => {
    return [
      fs
        .readFileSync(templateHtml)
        .toString()
        .replace(/{{(\w+)}}/g, (_, x) => dict[x] && dict[x]({ tip: props, filename })),
      path(filename)
    ]
  })
  .forEach(([html, path]) => {
    fs.mkdirSync(`./dist/${path}`)
    fs.writeFileSync(`./dist/${path}/index.html`, html, 'utf8')
  })
