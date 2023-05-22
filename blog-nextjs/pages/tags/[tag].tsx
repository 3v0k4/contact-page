import Head from 'next/head'
import Newsletter from '../../components/newsletter'
import { Posts } from '../../components/posts'
import { Props as TagsProps } from '../../components/tags'
import { getCategories, getTags, getPostsByTag, getAllPosts } from '../../lib/api'

type Post = {
  title: string,
  description: string,
  slug: string,
}

type Props = {
  posts: Post[],
  tag: string,
} & TagsProps

const Tag = ({ categories, tags, posts, tag }: Props) => (
  <>
    <Head>
      <title>{tag} - Riccardo Odone</title>
    </Head>

    <Posts categories={categories} tags={tags} posts={posts} />

    <Newsletter />
  </>
)

export default Tag

type Params = {
  params: {
    tag: string
  }
}

export const getStaticProps = async ({ params }: Params) => {
  const tag = params.tag
  const categories = getCategories()
  const tags = getTags()
  const posts = getPostsByTag(tag, ['title', 'description', 'slug'])

  return {
    props: { categories, tags, posts, tag },
  }
}

export const getStaticPaths = async () => {
  const posts = getAllPosts(['tags'])

  return {
    paths: posts.map(post => post.tags).flat().map(tag => {
      return {
        params: {
          tag
        },
      }
    }),
    fallback: false,
  }
}
