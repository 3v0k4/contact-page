import { InternalLink } from './internal-link'
import { Tags, Props as TagsProps } from './tags'

type Post = {
  title: string,
  description: string,
  slug: string,
}

type Props = {
  posts: Post[],
} & TagsProps

export const Posts = ({ categories, tags, posts }: Props) => (
  <div className="mx-auto max-w-3xl px-4 mt-10">
    <h1 className="text-center text-4xl font-semibold">
      Posts
    </h1>

    <div className="text-center mt-10">
      <Tags categories={categories} tags={tags} />
    </div>

    <ul className="list-none mt-10">
      {posts.map(({ title, description, slug }) => (
        <li key={slug} className="my-10">
          <InternalLink href={`/posts/${slug}`}>
            <a className="text-2xl text-[color:var(--blue)] hover:text-[color:var(--pink)]">{title}</a>
          </InternalLink>
          <p className="text-gray-500">{description}</p>
        </li>
      ))}
    </ul>
  </div>
)
