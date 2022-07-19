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
  <div className="tw-mx-auto tw-max-w-3xl tw-px-4 tw-mt-10">
    <h1 className="tw-text-center tw-text-4xl tw-font-semibold">
      Posts
    </h1>

    <div className="tw-text-center tw-mt-10">
      <Tags categories={categories} tags={tags} />
    </div>

    <ul className="tw-list-none tw-mt-10">
      {posts.map(({ title, description, slug }) => (
        <li key={slug} className="tw-my-10">
          <InternalLink href={`/posts/${slug}`}>
            <a className="tw-text-2xl tw-text-[color:var(--blue)] hover:tw-text-[color:var(--pink)]">{title}</a>
          </InternalLink>
          <p className="tw-text-gray-500">{description}</p>
        </li>
      ))}
    </ul>
  </div>
)
