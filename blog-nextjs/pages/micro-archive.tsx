import Head from 'next/head'
import Newsletter from '../components/newsletter'
import { MicroPosts } from '../components/micro-posts'
import { getMicroCategories, getMicroTags, getAllMicroPosts } from '../lib/api'
import markdownToHtml from '../lib/markdownToHtml'

type Tag = {
  tag: string,
  count: number,
}

type Category = Tag

type Post = {
  title: string,
  description: string,
  slug: string,
  content: string,
}

type Props = {
  categories: Category[],
  tags: Tag[],
  posts: Post[],
}

const MicroArchive = ({ categories, tags, posts }: Props) => (
  <>
    <Head>
      <title>MicroArchives - Riccardo Odone</title>
    </Head>

    <MicroPosts categories={categories} tags={tags} posts={posts} />

    <Newsletter />
  </>
)

export default MicroArchive

export const getStaticProps = async () => {
  const categories = getMicroCategories()
  const tags = getMicroTags()
  const posts = getAllMicroPosts(['title', 'description', 'slug', 'content'])
  posts.forEach(async (post) => post.content = await markdownToHtml(post.content || ''))

  return {
    props: { categories, tags, posts },
  }
}
