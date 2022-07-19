import Head from 'next/head'
import Link from 'next/link'
import Newsletter from '../../components/newsletter'
import markdownToHtml from '../../lib/markdownToHtml'
import { InternalLink } from '../../components/internal-link'
import { TagLink } from '../../components/tag-link'
import { getPostBySlug, getAllPosts } from '../../lib/api'
import { useRouter } from 'next/router'

type Post = {
  title: string,
  description: string,
  slug: string,
  cover_image: string,
  date: string,
  author: string,
  canonical_url: string,
  tags: string[],
  series: string,
  seriesPosts: { slug: string, title: string }[]
  randomPosts: { slug: string, title: string, description: string }[]
  content: string,
  tweet: string,
}

type Props = {
  post: Post
}

const Post = ({ post }: Props) => (
  <>
    <Head>
      <title>{post.title}</title>
      <meta property="og:title" content={post.title} />
      <meta property="og:description" content={post.description} />
      <meta name="description" content={post.description} />
      <meta property="og:type" content="article" />
      <meta property="og:image" content={post.cover_image} />
      <meta name="twitter:card" content="summary_large_image" />
    </Head>

    <div className="tw-mx-auto tw-max-w-3xl tw-px-4 tw-my-10">
      <article>
        { post.cover_image && <img className="tw-mb-10" src={post.cover_image} /> }

        <h1 className="tw-text-4xl tw-font-semibold">
          {post.title}
        </h1>

        <section className="tw-text-gray-500 tw-text-base tw-mb-10 tw-mt-4">
          <div className="tw-mb-4">
            Posted on {post.date}
            { post.author && <span> by {post.author}</span> }
            { post.canonical_url && <span><br />Originally posted at <Link href={post.canonical_url}><a target="_blank" rel="noopener" className="tw-underline">{post.canonical_url}</a></Link>.</span> }
          </div>

          <div>
            {post.tags.map(tag => <TagLink key={tag} tag={tag} klass="tw-text-sm tw-px-2 tw-py-1">{tag}</TagLink>)}
          </div>
        </section>

        {post.series && (
          <section className="tw-border tw-rounded-lg tw-border-black tw-mb-10">
            <h3 className="tw-px-3 tw-py-2">{post.series} (Series)</h3>
            {(post.seriesPosts || []).map(seriesPost => (
              <SeriesLink key={seriesPost.slug} slug={seriesPost.slug} title={seriesPost.title} />
            ))}
          </section>
        )}

        <section className="post" dangerouslySetInnerHTML={{ __html: post.content }} />

        { post.tweet && (
          <p style={{ display: 'none' }}>
            Support my work by <Link href={post.tweet}><a target="_blank" rel="noopener">tweeting</a></Link> this article! 🙏
          </p>
        )}
      </article>
    </div>

    <Newsletter />

    <div className="tw-py-20 tw-flex tw-flex-col md:tw-flex-row tw-bg-[color:var(--blue)] tw-shadow-[0_0_5px_var(--blue)] tw-items-start tw-justify-around tw-gap-10">
      { post.randomPosts.map(randomPost => (
        <div key={randomPost.title} className="tw-px-4 md:tw-max-w-[33%]">
          <InternalLink href={`/posts/${randomPost.slug}`}>
            <a className="tw-text-black hover:tw-text-black">
              <h3 className="tw-text-2xl tw-underline tw-font-semibold">{randomPost.title}</h3>
              <p className="tw-mt-5">{randomPost.description}</p>
            </a>
          </InternalLink>
        </div>
      ))}
    </div>
  </>
)

export default Post

type Params = {
  params: {
    slug: string
  }
}

export const getStaticProps = async ({ params }: Params) => {
  const post = getPostBySlug(params.slug, [
   'title',
   'cover_image',
   'author',
   'canonical_url',
   'date',
   'tags',
   'series',
   'seriesPosts',
   'content',
   'tweet',
   'randomPosts',
  ])
  const content = await markdownToHtml(post.content || '')

  return {
    props: {
      post: {
        ...post,
        content,
      },
    },
  }
}

export const getStaticPaths = async () => {
  const posts = getAllPosts(['slug'])

  return {
    paths: posts.map(post => {
      return {
        params: {
          slug: `${post.slug}`,
        },
      }
    }),
    fallback: false,
  }
}

const SeriesLink = ({ slug, title }: { slug: string, title: string }) => {
  const router = useRouter()
  const href = `/posts/${encodeURIComponent(slug)}`
  const isActive = href === router.asPath

  return(
    <InternalLink href={href}>
      <a className={`tw-block tw-border-t tw-border-black tw-py-1 tw-pl-3 tw-text-base ${isActive ? 'tw-bg-[color:var(--pink)] tw-text-white hover:tw-text-[color:var(--blue)] hover:tw-bg-transparent' : 'tw-text-[color:var(--blue)] hover:tw-bg-[color:var(--pink)] hover:tw-text-white'}`}>
        {title}
      </a>
    </InternalLink>
  )
}
