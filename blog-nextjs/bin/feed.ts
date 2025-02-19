import fs from "fs";
import { Feed } from "feed";
import {
  getPostSlugs,
  parsePost,
  getMicroPostSlugs,
  parseMicroPost,
} from "../lib/api";
import { TLD } from "../consts";

export const getBlogPostsData = () =>
  getPostSlugs()
    .map((post) => ({ ...parsePost(post), path: "posts" }))
    .concat(
      getMicroPostSlugs().map((post) => ({
        ...parseMicroPost(post),
        path: "micro-posts",
      }))
    )
    .sort((post1, post2) => (post1.date > post2.date ? -1 : 1));

export const generateRssFeed = () => {
  const posts = getBlogPostsData();
  const root = `https://${TLD}`;
  const date = new Date();
  const author = {
    name: "Riccardo Odone",
  };

  const feed = new Feed({
    title: "odone.me",
    description: "Rambling on software as a learning tool",
    id: root,
    link: root,
    image: `${root}/images/android-chrome-512x512.png`,
    favicon: `${root}/images/android-chrome-512x512.png`,
    copyright: `All rights reserved ${date.getFullYear()}, Riccardo Odone`,
    updated: date,
    generator: "Feed for Node.js",
    feedLinks: {
      atom: `${root}/atom.xml`,
    },
    author,
  });

  posts.slice(0, 10).forEach((post) => {
    const url = `${root}/${post.path}/${post.slug}/`;

    feed.addItem({
      title: post.title,
      id: url,
      link: url,
      description: post.description,
      content: post.content,
      author: [author],
      contributor: [author],
      date: post.date,
    });
  });

  fs.mkdirSync("./public/rss", { recursive: true });
  fs.writeFileSync("./public/atom.xml", feed.atom1());
};

(async () => await generateRssFeed())();
