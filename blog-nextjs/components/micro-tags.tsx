import { TagLink } from '../components/tag-link'

type Tag = {
  tag: string,
  count: number,
}

type Category = Tag

export type Props = {
  categories: Category[],
  tags: Tag[],
}

export const MicroTags = ({ categories, tags }: Props) => {
  const allTag =
    <TagLink key="All" href={`/micro-archive/`}>
      <span>All</span>
      <span className="ml-2 rounded-md font-light leading-6 px-1 bg-white text-[color:var(--blue)]">
        {categories.reduce((acc, category) => acc + category.count, 0)}
      </span>
    </TagLink>

  return (
    <div>
      <div className="flex flex-wrap">
        { [allTag].concat(categories.map(({ tag, count }) => (
          <TagLink key={tag} href={`/micro-tags/${tag}/`.replace(" ", "%20")}>
            <span>{tag}</span>
            <span className="ml-2 rounded-md font-light leading-6 px-1 bg-white text-[color:var(--blue)]">{count}</span>
          </TagLink>
        )))}
      </div>

      <div className="mt-4 flex flex-wrap">
        { tags.map(({ tag, count }) => (
          <TagLink key={tag} href={`/micro-tags/${tag}/`.replace(" ", "%20")} klass="text-sm py-1 px-2">
            <span className="mr-2">{tag}</span>
            <span className="rounded-md text-sm leading-4 px-1 bg-white text-[color:var(--blue)]">{count}</span>
          </TagLink>
        ))}
      </div>
    </div>
  )
}