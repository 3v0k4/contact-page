import Head from 'next/head'
import { InternalLink } from '../../components/internal-link'
import Link from 'next/link'
import Newsletter from '../../components/newsletter'
import markdownToHtml from '../../lib/markdownToHtml'
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

    <div className="col-sm-8 col-md-8 col-lg-6 mx-auto mt-4">
      <article>
        { post.cover_image && <img className="mb-4" src={post.cover_image} /> }

        <h1>{post.title}</h1>
        <section className="font-weight-light text-secondary smaller mb-4">
          <div className="mb-2">
            Posted on {post.date}
            { post.author && <span> by {post.author}</span> }
            { post.canonical_url && <span>Originally posted at <Link href={post.canonical_url}><a target="_blank" rel="noopener">{post.canonical_url}</a></Link>.</span> }
          </div>

          <div>
            {post.tags.map(tag => <TagLink key={tag} tag={tag} klass="btn-sm">{tag}</TagLink>)}
          </div>
        </section>

        {post.series && (
          <section className="series-container">
            <h3 className="series-heading">{post.series} (Series)</h3>
            {(post.seriesPosts || []).map(seriesPost => (
              <SeriesLink key={seriesPost.slug} slug={seriesPost.slug} title={seriesPost.title} />
            ))}
          </section>
        )}

        <section dangerouslySetInnerHTML={{ __html: post.content }} />

        { post.tweet && (
          <p style={{ display: 'none' }}>
            Support my work by <Link href={post.tweet}><a target="_blank" rel="noopener">tweeting</a></Link> this article! 🙏
          </p>
        )}
      </article>
    </div>

    <Newsletter />

    <div className="read-more-container container-fluid py-5">
      <div className="row">
        { post.randomPosts.map(randomPost => (
          <div key={randomPost.title} className="col-sm-8 col-md-8 col-lg-4 text-center px-4 mx-auto read-more-wrapper">
            <InternalLink href={`/posts/${randomPost.slug}`}>
              <a className="read-more">
                <h3>{randomPost.title}</h3>
                <p>{randomPost.description}</p>
              </a>
            </InternalLink>
          </div>
        ))}
      </div>
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
  const isActive = href === router.asPath.replaceAll('%E2%80%93', '–').replaceAll('%2B', '+')

  return(
    <InternalLink href={href}>
      <a className={`series-post ${isActive ? 'series-post-selected' : 'series-post-unselected'}`}>
        {title}
      </a>
    </InternalLink>
  )
}
