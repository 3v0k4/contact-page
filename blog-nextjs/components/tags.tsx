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


export const Tags = ({ categories, tags }: Props) => (
  <div>
    <div>
      { categories.map(({ icon, tag, count }) => (
        <TagLink key={tag} tag={tag}>
          <span className="mr-2 w-6" dangerouslySetInnerHTML={{ __html: icon }} />
          <span className="mr-2">{tag}</span>
          <span className="rounded-md font-light leading-6 px-1 bg-white text-[color:var(--blue)]">{count}</span>
        </TagLink>
      ))}
    </div>

    <div className="mt-4">
      { tags.map(({ icon, tag, count }) => (
        <TagLink key={tag} tag={tag} klass="text-sm py-1 px-2">
          <span className="mr-2 w-4" dangerouslySetInnerHTML={{ __html: icon }} />
          <span className="mr-2">{tag}</span>
          <span className="rounded-md text-sm leading-4 px-1 bg-white text-[color:var(--blue)]">{count}</span>
        </TagLink>
      ))}
    </div>
  </div>
)
