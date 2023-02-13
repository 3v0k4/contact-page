#!/usr/bin/env node

const fs = require('fs')
const readYaml = require('node-read-yaml')
const Handlebars = require("handlebars");

const html = fs.readFileSync(`${__dirname}/dist/template.html`).toString()
const tipsDir = `${__dirname}/public/tips`

const slug = (filename) =>
  filename.split('-').slice(1).join('-').split('.')[0]

const path = (filename) =>
  `/${slug(filename)}/`

const tips = fs
  .readdirSync(tipsDir)
  .sort((a, b) => Number(a.split('-')[0]) - Number(b.split('-')[0]))
  .map((filename, i, filenames) => [filename, { ...readYaml.sync(`${tipsDir}/${filename}`), previousTipPath: path(filenames.at(i-1)), nextTipPath: path(filenames.at((i+1)%filenames.length)) }])

const pathsByIndex = JSON.stringify(tips.map(([filename]) => path(filename)))

const dict = ({ tip, filename }) => ({
  title: `${tip.title} | TypeScript Tips`,
  description: tip.description,
  tipTitle: tip.title,
  tipDescription: tip.description,
  canonical: `https://typescript.tips/${encodeURIComponent(slug(filename))}/`,
  version: new Date().getTime(),
  previousTipPath: tip.previousTipPath,
  nextTipPath: tip.nextTipPath,
  badCode: tip.badCode,
  goodCode: tip.goodCode,
  pathsByIndex
})

const dictIndex = ([filename, tip]) => ({
  ...dict({ tip, filename }),
  title: "TypeScript Tips",
  description: "TypeScript Tips | Make bugs impossible. One TypeScript tip at a time.",
  canonical: "https://typescript.tips",
  previousTipPath: path(tips.at(-1)[0]),
  nextTipPath: path(tips[0][0]),
})

fs.writeFileSync(`./dist/index.html`, Handlebars.compile(html)(dictIndex(tips[10])), 'utf8')

tips
  .forEach(([filename, props]) => {
    fs.mkdirSync(`./dist/${slug(filename)}`)
    fs.writeFileSync(`./dist/${slug(filename)}/index.html`, Handlebars.compile(html)(dict({ tip: props, filename })), 'utf8')
  })
