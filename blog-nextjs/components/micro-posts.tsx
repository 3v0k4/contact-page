import { InternalLink } from './internal-link'
import { MicroTags, Props as MicroTagsProps } from './micro-tags'

type Post = {
  title: string,
  description: string,
  slug: string,
  content: string,
}

type Props = {
  posts: Post[],
} & MicroTagsProps

export const MicroPosts = ({ categories, tags, posts }: Props) => (
  <div className="mx-auto max-w-3xl px-4 mt-10 break-all">
    <h1 className="text-center text-4xl font-semibold">
      MicroPosts
    </h1>

    <div className="text-center mt-10">
      <MicroTags categories={categories} tags={tags} />
    </div>

    <ul className="flex flex-col gap-20 list-none my-10">
      {posts.map(({ title, description, slug, content }) => (
        <li key={slug} className="pb-5 border-b border-gray-400 last:border-0">
          <InternalLink href={`/micro-posts/${slug}`}>
            <a className="text-4xl text-[color:var(--blue)] hover:text-[color:var(--pink)]">{title}</a>
          </InternalLink>
          <p className="text-2xl text-gray-500">{description}</p>
          <section className="mt-6 post" dangerouslySetInnerHTML={{ __html: content }} />
        </li>
      ))}
    </ul>
  </div>
)
