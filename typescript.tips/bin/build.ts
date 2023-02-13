import fs from "fs";
import { parse } from "yaml";
import Handlebars from "handlebars";

const html = fs.readFileSync(`${__dirname}/../src/template.html`).toString();
const tipsDir = `${__dirname}/../tips`;

const slug = (filename: string): string =>
  filename.split("-").slice(1).join("-").split(".")[0];

const path = (filename: string): string => `/${slug(filename)}/`;

const at = <T>(i: number, xs: T[]): T => xs.at(i % xs.length) as T;

const tips: [string, Record<string, unknown>][] = fs
  .readdirSync(tipsDir)
  .sort((a, b) => Number(a.split("-")[0]) - Number(b.split("-")[0]))
  .map((filename, i, filenames) => [
    filename,
    {
      ...parse(fs.readFileSync(`${tipsDir}/${filename}`).toString()),
      previousTipPath: path(at(i - 1, filenames)),
      nextTipPath: path(at(i + 1, filenames)),
    },
  ]);

const pathsByIndex = JSON.stringify(tips.map(([filename]) => path(filename)));

const dict = ({
  tip,
  filename,
}: {
  tip: Record<string, unknown>;
  filename: string;
}) => ({
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
  pathsByIndex,
});

const dictIndex = ([filename, tip]: [string, Record<string, unknown>]) => ({
  ...dict({ tip, filename }),
  title: "TypeScript Tips",
  description:
    "TypeScript Tips | Make bugs impossible. One TypeScript tip at a time.",
  canonical: "https://typescript.tips",
  previousTipPath: path(at(-1, tips)[0]),
  nextTipPath: path(tips[0][0]),
});

fs.writeFileSync(
  `./dist/index.html`,
  Handlebars.compile(html)(dictIndex(at(10, tips))),
  "utf8"
);

tips.forEach(([filename, props]: [string, Record<string, unknown>]) => {
  fs.mkdirSync(`./dist/${slug(filename)}`, { recursive: true });
  fs.writeFileSync(
    `./dist/${slug(filename)}/index.html`,
    Handlebars.compile(html)(dict({ tip: props, filename })),
    "utf8"
  );
});
