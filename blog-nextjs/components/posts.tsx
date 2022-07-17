import Link from 'next/link'
import { TagLink } from '../components/tag-link'

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

export const Posts = ({ categories, tags, posts }: Props) => (
  <div className="col-sm-8 col-md-8 col-lg-6 mx-auto mt-4">
    <h1 className="text-center">Posts</h1>

    <div className="text-center mt-4">
      <div>
        { categories.map(({ icon, tag, count }) => (
          <TagLink key={tag} tag={tag}>
            <span className="btn-tag-icon-lg">{icon} </span>{tag} <span className="badge badge-white">{count}</span>
          </TagLink>
        ))}
      </div>

      <div className="mt-3 d-none d-md-block">
        { tags.map(({ icon, tag, count }) => (
          <TagLink key={tag} tag={tag} klass="btn-sm">
            <span className="btn-tag-icon-lg">{icon} </span>{tag} <span className="badge badge-white badge-sm">{count}</span>
          </TagLink>
        ))}
      </div>
    </div>

    <ul className="no-list-style mt-5">
      {posts.map(({ title, description, slug }) => (
        <li key={slug} className="my-5">
          <div><Link href={`/posts/${slug}${process.env.NODE_ENV === 'production' ? '.html' : ''}`}><a className="post-title">{title}</a></Link></div>
          <p className="text-secondary">{description}</p>
        </li>
      ))}
    </ul>
  </div>
)
