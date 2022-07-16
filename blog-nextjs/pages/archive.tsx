import Head from 'next/head'
import Newsletter from '../components/newsletter'
import { Posts } from '../components/posts'
import { getCategories, getTags, getAllPosts } from '../lib/api'

type Tag = {
  icon: string,
  tag: string,
  count: number,
}

type Category = Tag

type Post = {
  title: string,
  description: string,
  slug: string,
}

type Props = {
  categories: Category[],
  tags: Tag[],
  posts: Post[],
}

const Archive = ({ categories, tags, posts }: Props) => (
  <>
    <Head>
      <title>Riccardo Odone - Archives</title>
    </Head>

    <Posts categories={categories} tags={tags} posts={posts} />

    <Newsletter />
  </>
)

export default Archive

export const getStaticProps = async () => {
  const categories = getCategories()
  const tags = getTags()
  const posts = getAllPosts(['title', 'description', 'slug'])

  return {
    props: { categories, tags, posts },
  }
}
