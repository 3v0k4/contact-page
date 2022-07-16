import fs from 'fs'
import { Feed } from 'feed'
import { getPostSlugs, parsePost } from '../lib/api'

export const getBlogPostsData = async () =>
  getPostSlugs()
    .map(parsePost)
    .sort((post1, post2) => (post1.date > post2.date ? -1 : 1))

export const generateRssFeed = async () => {
  const posts = await getBlogPostsData()
  const root = 'https://odone.io'
  const date = new Date()
  const author = {
    name: 'Riccardo Odone',
    email: 'riccardo(at)odone.io',
  }

  const feed = new Feed({
    title: 'odone.io',
    description: 'Rambling on software as a learning tool',
    id: root,
    link: root,
    image: `${root}/images/android-chrome-512x512.png`,
    favicon: `${root}/images/android-chrome-512x512.png`,
    copyright: `All rights reserved ${date.getFullYear()}, Riccardo Odone`,
    updated: date,
    generator: 'Feed for Node.js',
    feedLinks: {
      atom: `${root}/atom.xml`,
    },
    author,
  })

  posts.slice(0, 10).forEach(post => {
    const url = `${root}/posts/${post.slug}.html`

    feed.addItem({
      title: post.title,
      id: url,
      link: url,
      description: post.description,
      content: post.content,
      author: [author],
      contributor: [author],
      date: post.date,
    })
  })

  fs.mkdirSync('./public/rss', { recursive: true })
  fs.writeFileSync('./public/atom.xml', feed.atom1())
}

(async () => await generateRssFeed())()
