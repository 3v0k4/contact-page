import fs from "fs";
import yaml from "yaml";

const slugFrom = (filename: string): string => {
  const slug = filename.split("-").slice(1).join("-").split(".")[0];
  if (!slug) throw new Error(`Invalid filename: ${filename}`);
  return slug;
};

const path = (filename: string): string =>
  `/${encodeURIComponent(slugFrom(filename))}/`;

const at = <T extends unknown>(i: number, xs: T[]): T =>
  xs.at(i % xs.length) as T;

const tips = fs
  .readdirSync("./tips")
  .sort((a, b) => Number(a.split("-")[0]) - Number(b.split("-")[0]));

export type Tip = {
  previousTipPath: string;
  nextTipPath: string;
  title: string;
  description: string;
  badCode: string;
  goodCode: string;
  twitterHandle: string;
};

export type Page = {
  canonical: string;
  slug: string;
} & Tip;

export const readPages = (): Page[] => {
  return tips.map((filename, i, filenames) => {
    const parsed = yaml.parse(fs.readFileSync(`./tips/${filename}`).toString());

    return {
      ...parsed,
      canonical: `https://typescript.tips${path(filename)}`,
      previousTipPath: path(at(i - 1, filenames)),
      nextTipPath: path(at(i + 1, filenames)),
      slug: slugFrom(filename),
      twitterHandle: parsed.twitterHandle || "RiccardoOdone",
    };
  });
};

export type PageLink = {
  canonical: string;
  title: string;
}

export const pageLinksByIndex: PageLink[] =
  tips.map((filename) => {
    const parsed = yaml.parse(fs.readFileSync(`./tips/${filename}`).toString());

    return {
      canonical: `https://typescript.tips${path(filename)}`,
      title: parsed.title,
    };
  });