import fs from 'fs';
import { parse } from 'yaml';

const slugFrom = (filename: string): string => {
  const slug = filename.split("-").slice(1).join("-").split(".")[0];
  if (!slug) throw new Error(`Invalid filename: ${filename}`);
  return slug;
}

const path = (filename: string): string => `/${encodeURIComponent(slugFrom(filename))}/`;

const at = <T extends unknown>(i: number, xs: T[]): T => xs.at(i % xs.length) as T;

const tips = fs
  .readdirSync('./tips')
  .sort((a, b) => Number(a.split("-")[0]) - Number(b.split("-")[0]))

export type Tip = {
  previousTipPath: string;
  nextTipPath: string;
  pathsByIndex: string[];
  title: string;
  description: string;
  badCode: string;
  goodCode: string;
}

type Page = {
  canonical: string;
  slug: string;
} & Tip;


export const readTips = (): Page[] => {
  return tips
    .map((filename, i, filenames) => {
      const yaml = fs.readFileSync(`./tips/${filename}`).toString();

      return  {
        ...parse(yaml),
        canonical: `https://typescript.tips${path(filename)}`,
        previousTipPath: path(at(i - 1, filenames)),
        nextTipPath: path(at(i + 1, filenames)),
        pathsByIndex: tips.map((filename) => path(filename)),
        slug: slugFrom(filename),
      };
    });
}
