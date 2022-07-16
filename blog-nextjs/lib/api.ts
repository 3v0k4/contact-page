import fs from 'fs'
import matter from 'gray-matter'
import { join } from 'path'

type Items = Record<string, string>

type Post = {
  title: string,
  description: string,
  slug: string,
  cover_image: string,
  date: Date,
  author: string,
  canonical_url: string,
  tags: string[],
  series: string,
  seriesPosts: { slug: string, title: string }[]
  content: string,
  tweet: string,
}

const POSTS_DIRECTORY = join(process.cwd(), '_posts')

export const parsePost = (slug: string): Record<string, any> => {
  const fullPath = join(POSTS_DIRECTORY, `${slug}.md`)
  const fileContents = fs.readFileSync(fullPath, 'utf8')
  const date = new Date(slug.split('-').slice(0, 3).join('-'))
  const { data, content } = matter(fileContents)
  return { ...data, slug, date, content }
}

export const getPostSlugs = () =>
  fs.readdirSync(POSTS_DIRECTORY).map(slug => slug.replace(/\.md$/, ''))

const randomIndex = (max: number, randoms: number[]): number => {
  const newRandom = Math.floor(Math.random() * max)
  if (randoms.find(element => element === newRandom)) randomIndex(max, randoms)
  return newRandom
}

const randomIndexes = (length: number, xs: unknown[]): number[] =>
  Array
    .from({ length })
    .reduce((acc: any) => [...acc, randomIndex(xs.length, acc)], []) as number[]

const randomElements = <T>(length: number, xs: T[]): T[] =>
  randomIndexes(length, xs)
    .map(index => xs[index])

const getPostBySlug_ = (slug: string, fields: string[] = []): Post => {
  const post = parsePost(slug)

  const seriesPosts = getPostSlugs()
    .map(parsePost)
    .filter((p: any) => p.series && p.series === post.series)
    .map((p: any) => ({ slug: p.slug.replace(/\.md$/, ''), title: p.title }))

  const randomPosts = randomElements(3, getPostSlugs())
    .map(parsePost)
    .map((p: any) => ({ slug: p.slug.replace(/\.md$/, ''), title: p.title, description: p.description }))

  return fields.length === 0 ?
    { ...post, seriesPosts } :
    fields.reduce((acc: any, field) => {
      if (field === 'seriesPosts') { return { ...acc, [field]: seriesPosts } }
      if (field === 'randomPosts') { return { ...acc, [field]: randomPosts } }
      if (typeof post[field] !== 'undefined') { return { ...acc, [field]: post[field] } }
      return acc
    }, {})
  }

const toStringifiedDate = (post: any): any =>
  ({ ...post, date: post.date.toLocaleDateString('en', { year: 'numeric', month: 'long', day: 'numeric' }) })

export const getPostBySlug = (slug: string, fields: string[] = []): any =>
  toStringifiedDate(getPostBySlug_(slug, fields))

export const getAllPosts = (fields: string[] = []) =>
  getPostSlugs()
    .map((slug) => getPostBySlug_(slug, fields.length === 0 ? fields : fields.concat(['date'])))
    .sort((post1, post2) => (post1.date > post2.date ? -1 : 1))
    .map(toStringifiedDate)

export const getPostsByTag = (tag: string, fields: string[] = []) =>
  getPostSlugs()
    .map((slug) => getPostBySlug_(slug, fields.length === 0 ? fields : fields.concat(['date', 'tags'])))
    .filter(post => post.tags.includes(tag))
    .sort((post1, post2) => (post1.date > post2.date ? -1 : 1))
    .map(toStringifiedDate)

const ICONS: Record<string, string> = {
  "Functional Programming": '♕',
  "Essential Skills": '♔',
}

const toTally = (acc: any, category: any) => {
  if (acc[category]) { return { ...acc, [category]: acc[category] + 1 } }
  return { ...acc, [category]: 1 }
}

export const getCategories = () => {
  const tally = getAllPosts()
    .map(post => post.tags)
    .map(tags => tags[0])
    .reduce(toTally, {})

  return Object
    .entries(tally)
    .map(([tag, count]) => ({ icon: ICONS[tag], tag, count }))
    .sort((a, b) => a.tag.localeCompare(b.tag))
}

export const getTags = () => {
  const tally = getAllPosts()
    .map(post => post.tags)
    .map((tags: any) => tags.slice(1).map((tag: any) => [tags[0], tag]))
    .flat()
    .reduce(toTally, {})

  return Object
    .entries(tally)
    .map(([category, count]) => ({ icon: ICONS[category.split(',')[0]], tag: category.split(',')[1], count }))
    .sort((a, b) => a.tag.localeCompare(b.tag))
}
