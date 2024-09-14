import { TagLink } from '../components/tag-link'

type Tag = {
  icon: string,
  tag: string,
  count: number,
}

type Category = Tag

export type Props = {
  categories: Category[],
  tags: Tag[],
}

export const Tags = ({ categories, tags }: Props) => {
  const allTag =
    <TagLink key="All" href={`/archive/`}>
      <span>All</span>
      <span className="ml-2 rounded-md font-light leading-6 px-1 bg-white text-gray-500">
        {categories.reduce((acc, category) => acc + category.count, 0)}
      </span>
    </TagLink>

  return (
    <div>
      <div className="flex flex-wrap gap-2">
        { [allTag].concat(categories.map(({ icon, tag, count }) => (
          <TagLink key={tag} href={`/tags/${tag}/`.replace(" ", "%20")}>
            <span className="mr-2 w-6" dangerouslySetInnerHTML={{ __html: icon }} />
            <span>{tag}</span>
            <span className="ml-2 rounded-md font-light leading-6 px-1 bg-white text-gray-500">{count}</span>
          </TagLink>
        )))}
      </div>

      <div className="mt-4 flex flex-wrap gap-2">
        { tags.map(({ icon, tag, count }) => (
          <TagLink key={tag} href={`/tags/${tag}/`.replace(" ", "%20")} klass="text-sm py-1 px-2">
            <span className="mr-2 w-4" dangerouslySetInnerHTML={{ __html: icon }} />
            <span className="mr-2">{tag}</span>
            <span className="rounded-md text-sm leading-4 px-1 bg-white text-gray-500">{count}</span>
          </TagLink>
        ))}
      </div>
    </div>
  )
}
