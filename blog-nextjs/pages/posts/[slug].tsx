import Head from 'next/head'
import Link from 'next/link'
import Newsletter from '../../components/newsletter'
import markdownToHtml from '../../lib/markdownToHtml'
import { InternalLink } from '../../components/internal-link'
import { TagLink } from '../../components/tag-link'
import { getPostBySlug, getAllPosts } from '../../lib/api'
import { useRouter } from 'next/router'
import { useEffect } from 'react'
import { TLD } from '../../consts'

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

const Post = ({ post }: Props) => {
  const title = `${post.title} - Riccardo Odone`
  useEffect(() => {
    const s = document.createElement("script");
    s.setAttribute("src", "https://platform.twitter.com/widgets.js");
    s.setAttribute("async", "true");
    document.head.appendChild(s);
  }, []);

  const coverImage = post.cover_image ? (post.cover_image.startsWith("/") ? `https://${TLD}${post.cover_image}` : post.cover_image) : null

  return <>
    <Head>
      <title>{title}</title>
      <meta key="og:title" property="og:title" content={post.title} />
      <meta key="og:description" property="og:description" content={post.description} />
      <meta key="description" name="description" content={post.description} />
      <meta key="og:type" property="og:type" content="article" />
      { coverImage && <meta key="og:image" property="og:image" content={coverImage} /> }
      <meta key="twitter:card" name="twitter:card" content="summary_large_image" />
    </Head>

    <div className="mx-auto max-w-3xl px-4 my-10">
      <article>
        { coverImage && <img className="mb-10" src={coverImage} /> }

        <h1 className="text-4xl font-semibold">
          {post.title}
        </h1>

        <section className="text-gray-500 text-base mb-10 mt-4">
          <div className="mb-4">
            Posted on {post.date}
            { post.author && <span> by {post.author}</span> }
            { post.canonical_url && <span><br />Originally posted at <Link
              href={post.canonical_url}
              target="_blank"
              rel="noopener"
              className="underline">{post.canonical_url}</Link>.</span> }
          </div>

          <div className="flex flex-wrap gap-2">
            {post.tags.map(tag => <TagLink key={tag} href={`/tags/${tag}/`.replace(" ", "%20")} klass="text-sm px-2 py-1">{tag}</TagLink>)}
          </div>
        </section>

        {post.series && (
          <section className="border rounded-lg border-black mb-10">
            <h3 className="px-3 py-2">{post.series} (Series)</h3>
            {(post.seriesPosts || []).map(seriesPost => (
              <SeriesLink key={seriesPost.slug} slug={seriesPost.slug} title={seriesPost.title} />
            ))}
          </section>
        )}

        <section className="post" dangerouslySetInnerHTML={{ __html: post.content.replaceAll("{{TLD}}", TLD) }} />

        { post.tweet && (
          <p style={{ display: 'none' }}>
            Support my work by <Link href={post.tweet} target="_blank" rel="noopener">tweeting</Link> this article! 🙏
          </p>
        )}
      </article>
    </div>

    <Newsletter />

    <div className="py-20 flex flex-col md:flex-row bg-[color:var(--blue)] shadow-[0_0_5px_var(--blue)] items-start justify-around gap-10">
      { post.randomPosts.map(randomPost => (
        <div key={randomPost.title} className="px-4 md:max-w-[33%]">
          <InternalLink href={`/posts/${randomPost.slug}`}>
            <a className="text-black hover:text-black">
              <h3 className="text-2xl underline font-semibold">{randomPost.title}</h3>
              <p className="mt-5">{randomPost.description}</p>
            </a>
          </InternalLink>
        </div>
      ))}
    </div>
  </>
}

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
  const href = `/posts/${encodeURIComponent(slug)}/`
  const isActive = href === router.asPath

  return(
    <InternalLink href={href}>
      <a className={`block border-t border-black py-1 pl-3 text-base ${isActive ? 'bg-[color:var(--pink)] text-white hover:text-[color:var(--blue)] hover:bg-transparent' : 'text-[color:var(--blue)] hover:bg-[color:var(--pink)] hover:text-white'}`}>
        {title}
      </a>
    </InternalLink>
  )
}
