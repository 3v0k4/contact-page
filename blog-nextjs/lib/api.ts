import fs from "fs";
import matter from "gray-matter";
import { join } from "path";

type Items = Record<string, string>;

type Post = {
  title: string;
  description: string;
  slug: string;
  cover_image: string;
  date: Date;
  author: string;
  canonical_url: string;
  tags: string[];
  series: string;
  seriesPosts: { slug: string; title: string }[];
  randomPosts: { slug: string; tile: string; description: string }[];
  content: string;
  tweet: string;
};

type SerializablePost = Omit<Post, "date"> & { date: string };

type ParsedPost = Partial<Post> & { slug: string } & { date: Date } & {
  title: string;
};

const POSTS_DIRECTORY = join(process.cwd(), "_posts");

export const parsePost = (slug: string) => {
  const fullPath = join(POSTS_DIRECTORY, `${slug}.md`);
  const fileContents = fs.readFileSync(fullPath, "utf8");
  const date = new Date(slug.split("-").slice(0, 3).join("-"));
  const { data, content } = matter(fileContents);
  return { ...data, slug, date, content } as ParsedPost;
};

export const getPostSlugs = () =>
  fs.readdirSync(POSTS_DIRECTORY).map((slug) => slug.replace(/\.md$/, ""));

const randomIndex = (max: number, randoms: number[]): number => {
  const newRandom = Math.floor(Math.random() * max);
  if (randoms.find((element) => element === newRandom))
    randomIndex(max, randoms);
  return newRandom;
};

const randomIndexes = (length: number, xs: unknown[]): number[] =>
  Array.from({ length }).reduce(
    (acc: number[]) => [...acc, randomIndex(xs.length, acc)],
    []
  );

const randomElements = <T>(length: number, xs: T[]): T[] =>
  randomIndexes(length, xs).map((index) => xs[index]);

type Fields = (keyof Post)[];

const getPostBySlug_ = (slug: string, fields: Fields = []): Post => {
  const post = parsePost(slug);

  const seriesPosts = getPostSlugs()
    .map(parsePost)
    .filter((p) => p.series && p.series === post.series)
    .map((p) => ({ slug: p.slug.replace(/\.md$/, ""), title: p.title }));

  const randomPosts = randomElements(3, getPostSlugs())
    .map(parsePost)
    .map((p) => ({
      slug: p.slug.replace(/\.md$/, ""),
      title: p.title,
      description: p.description,
    }));

  return fields.length === 0
    ? (post as Post)
    : (fields.reduce((acc, field) => {
        if (field === "seriesPosts") {
          return { ...acc, [field]: seriesPosts };
        }
        if (field === "randomPosts") {
          return { ...acc, [field]: randomPosts };
        }
        if (typeof post[field] !== "undefined") {
          return { ...acc, [field]: post[field] };
        }
        return acc;
      }, {}) as Post);
};

const toStringifiedDate = (post: Post): SerializablePost => ({
  ...post,
  date: post.date.toLocaleDateString("en", {
    year: "numeric",
    month: "long",
    day: "numeric",
  }),
});

export const getPostBySlug = (
  slug: string,
  fields: Fields = []
): SerializablePost => toStringifiedDate(getPostBySlug_(slug, fields));

export const getAllPosts = (fields: Fields = []) =>
  getPostSlugs()
    .map((slug) =>
      getPostBySlug_(
        slug,
        fields.length === 0 ? fields : fields.concat(["date"])
      )
    )
    .sort((post1, post2) => (post1.date > post2.date ? -1 : 1))
    .map(toStringifiedDate);

export const getPostsByTag = (tag: string, fields: Fields = []) =>
  getPostSlugs()
    .map((slug) =>
      getPostBySlug_(
        slug,
        fields.length === 0 ? fields : fields.concat(["date", "tags"])
      )
    )
    .filter((post) => post.tags.includes(tag))
    .sort((post1, post2) => (post1.date > post2.date ? -1 : 1))
    .map(toStringifiedDate);

const ICONS: Record<string, string> = {
  "Functional Programming": `
<svg fill="inherit" class="w-5 h-5" version="1.1" viewBox="0 0 700 600" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
 <path d="m613.62 261.39c10.176 10.176 10.176 26.941 0 37.117l-245.11 245.11c-10.176 10.176-26.941 10.176-37.117 0l-245.01-245.01c-10.176-10.176-10.176-26.941 0-37.117l245.1-245.11c10.176-10.176 26.941-10.176 37.117 0z" fill-rule="evenodd"/>
</svg>
`,
  "Essential Skills": `
<svg fill="inherit" class="w-5 h-5" version="1.1" viewBox="0 0 700 600" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <path d="m350 8.75c-11.84 0-22.297 8.0117-25.363 19.449l-44.945 166.57c-1.0352 3.8672-4.4414 6.4922-8.4414 6.4922l-166.25-0.015625c-11.602 0-21.91 7.7031-25.191 18.832-3.2773 11.129 1.2227 23.191 10.973 29.48l132.7 84.32c3.4531 2.207 4.9023 6.3672 3.5547 10.238l-59.32 172.21c-3.8828 11.121 0.18359 23.574 9.8789 30.266 9.6953 6.6914 22.77 6.0703 31.785-1.5039l135.03-114.33c1.5742-1.3047 3.5469-2.0156 5.5898-2.0156 2.0469 0 4.0156 0.71094 5.5898 2.0156l135.03 114.33c9.0195 7.5742 22.094 8.1953 31.785 1.5039 9.6953-6.6914 13.762-19.145 9.8789-30.266l-59.32-172.21c-1.3477-3.8711 0.10156-8.0312 3.5547-10.238l132.7-84.32c9.75-6.2891 14.25-18.352 10.973-29.48-3.2812-11.129-13.59-18.832-25.191-18.832l-166.25 0.015625c-4.0039 0-7.4062-2.6289-8.4414-6.4922l-44.945-166.57c-3.0664-11.438-13.523-19.449-25.363-19.449z" fill-rule="evenodd"/>
</svg>
`,
};

const toTally = (acc: Record<string, number>, label: string) => {
  if (acc[label]) {
    return { ...acc, [label]: acc[label] + 1 };
  }
  return { ...acc, [label]: 1 };
};

export const getCategories = () => {
  const tally = getAllPosts()
    .map((post) => post.tags)
    .map((tags) => tags[0])
    .reduce(toTally, {});

  return Object.entries(tally)
    .map(([tag, count]) => ({ icon: ICONS[tag], tag, count }))
    .sort((a, b) => a.tag.localeCompare(b.tag));
};

export const getTags = () => {
  const tally = getAllPosts()
    .map((post) => post.tags)
    .map((tags) => tags.slice(1).map((tag) => [tags[0], tag].join(",")))
    .flat()
    .reduce(toTally, {});

  return Object.entries(tally)
    .map(([category, count]) => ({
      icon: ICONS[category.split(",")[0]],
      tag: category.split(",")[1],
      count,
    }))
    .sort((a, b) => a.tag.localeCompare(b.tag));
};

const MICROPOSTS_DIRECTORY = join(process.cwd(), "_micro-posts");

export const getMicroPostSlugs = () =>
  fs.readdirSync(MICROPOSTS_DIRECTORY).map((slug) => slug.replace(/\.md$/, ""));

export const parseMicroPost = (slug: string) => {
  const fullPath = join(MICROPOSTS_DIRECTORY, `${slug}.md`);
  const fileContents = fs.readFileSync(fullPath, "utf8");
  const date = new Date(slug.split("-").slice(0, 3).join("-"));
  const { data, content } = matter(fileContents);
  return { ...data, slug, date, content } as ParsedPost;
};

const getMicroPostBySlug_ = (slug: string, fields: Fields = []): Post => {
  const post = parseMicroPost(slug);

  const seriesPosts = getMicroPostSlugs()
    .map(parseMicroPost)
    .filter((p) => p.series && p.series === post.series)
    .map((p) => ({ slug: p.slug.replace(/\.md$/, ""), title: p.title }));

  const randomPosts = randomElements(3, getMicroPostSlugs())
    .map(parseMicroPost)
    .map((p) => ({
      slug: p.slug.replace(/\.md$/, ""),
      title: p.title,
      description: p.description,
    }));

  return fields.length === 0
    ? (post as Post)
    : (fields.reduce((acc, field) => {
        if (field === "seriesPosts") {
          return { ...acc, [field]: seriesPosts };
        }
        if (field === "randomPosts") {
          return { ...acc, [field]: randomPosts };
        }
        if (typeof post[field] !== "undefined") {
          return { ...acc, [field]: post[field] };
        }
        return acc;
      }, {}) as Post);
};

export const getAllMicroPosts = (fields: Fields = []) =>
  getMicroPostSlugs()
    .map((slug) =>
      getMicroPostBySlug_(
        slug,
        fields.length === 0 ? fields : fields.concat(["date"])
      )
    )
    .sort((post1, post2) => (post1.date > post2.date ? -1 : 1))
    .map(toStringifiedDate);

export const getMicroCategories = () => {
  const tally = getAllMicroPosts()
    .map((post) => post.tags)
    .map((tags) => tags[0])
    .reduce(toTally, {});

  return Object.entries(tally)
    .map(([tag, count]) => ({ icon: null, tag, count }))
    .sort((a, b) => a.tag.localeCompare(b.tag));
};

export const getMicroTags = () => {
  const tally = getAllMicroPosts()
    .map((post) => post.tags)
    .map((tags) => tags.slice(1).map((tag) => [tags[0], tag].join(",")))
    .flat()
    .reduce(toTally, {});

  return Object.entries(tally)
    .map(([category, count]) => ({
      icon: null,
      tag: category.split(",")[1],
      count,
    }))
    .sort((a, b) => a.tag.localeCompare(b.tag));
};

export const getMicroPostsByTag = (tag: string, fields: Fields = []) =>
  getMicroPostSlugs()
    .map((slug) =>
      getMicroPostBySlug_(
        slug,
        fields.length === 0 ? fields : fields.concat(["date", "tags"])
      )
    )
    .filter((post) => post.tags.includes(tag))
    .sort((post1, post2) => (post1.date > post2.date ? -1 : 1))
    .map(toStringifiedDate);

export const getMicroPostBySlug = (
  slug: string,
  fields: Fields = []
): SerializablePost => toStringifiedDate(getMicroPostBySlug_(slug, fields));
