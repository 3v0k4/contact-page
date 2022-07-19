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
          <span className="tw-mr-2 tw-w-6" dangerouslySetInnerHTML={{ __html: icon }} />
          <span className="tw-mr-2">{tag}</span>
          <span className="tw-rounded-md tw-font-light tw-leading-6 tw-px-1 tw-bg-white tw-text-[color:var(--blue)]">{count}</span>
        </TagLink>
      ))}
    </div>

    <div className="tw-mt-4">
      { tags.map(({ icon, tag, count }) => (
        <TagLink key={tag} tag={tag} klass="tw-text-sm tw-py-1 tw-px-2">
          <span className="tw-mr-2 tw-w-4" dangerouslySetInnerHTML={{ __html: icon }} />
          <span className="tw-mr-2">{tag}</span>
          <span className="tw-rounded-md tw-text-sm tw-leading-4 tw-px-1 tw-bg-white tw-text-[color:var(--blue)]">{count}</span>
        </TagLink>
      ))}
    </div>
  </div>
)
