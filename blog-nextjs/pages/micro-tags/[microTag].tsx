import Head from 'next/head'
import Newsletter from '../../components/newsletter'
import { MicroPosts } from '../../components/micro-posts'
import { Props as MicroTagsProps } from '../../components/micro-tags'
import { getMicroCategories, getMicroTags, getMicroPostsByTag, getAllMicroPosts } from '../../lib/api'
import markdownToHtml from '../../lib/markdownToHtml'

type Post = {
  title: string,
  description: string,
  slug: string,
  content: string,
}

type Props = {
  posts: Post[],
  tag: string,
} & MicroTagsProps

const Tag = ({ categories, tags, posts, tag }: Props) => {
  const title = `${tag} - Riccardo Odone`

  return <>
    <Head>
      <title>{title}</title>
    </Head>

    <MicroPosts categories={categories} tags={tags} posts={posts} />

    <Newsletter />
  </>
}

export default Tag

type Params = {
  params: {
    microTag: string
  }
}

export const getStaticProps = async ({ params }: Params) => {
  const tag = params.microTag
  const categories = getMicroCategories()
  const tags = getMicroTags()
  const posts = getMicroPostsByTag(tag, ['title', 'description', 'slug', 'content'])
  posts.forEach(async (post) => post.content = await markdownToHtml(post.content || ''))

  return {
    props: { categories, tags, posts, tag },
  }
}

export const getStaticPaths = async () => {
  const posts = getAllMicroPosts(['tags'])

  return {
    paths: posts.map(post => post.tags).flat().map(microTag => {
      return {
        params: {
          microTag
        },
      }
    }),
    fallback: false,
  }
}
