#!/usr/bin/env node

const fs = require('fs')

const tipsDir = `${__dirname}/public/tips`

const toXml = (path) => `
  <url>
    <loc>https://typescript.tips/${path}</loc>
  </url>
`

const layout = (html) => `<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
  ${html}
</urlset>
`

const xml = layout(
  fs
  .readdirSync(tipsDir)
  .sort((a, b) => Number(a.split('-')[0]) - Number(b.split('-')[0]))
  .map((filename) => encodeURIComponent(filename))
  .map((filename) => toXml(filename.split('-').slice(1).join('-').split('.')[0]))
  .join('')
)

fs.writeFile('./dist/sitemap.xml', xml, 'utf8', (err) => {
  if (err) { return console.log(err) }
})
