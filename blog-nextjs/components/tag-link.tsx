import { InternalLink } from '../components/internal-link'
import React from 'react'
import { useRouter } from 'next/router'

type Props = React.PropsWithChildren<{tag: string, klass: string}>

export const TagLink = ({ tag, klass, children }: Props) => {
  const router = useRouter()
  const href = `/tags/${tag.replaceAll(/\s/g, '+')}`
  const isActive = href === router.asPath.replaceAll('%2B', '+')

  return (
    <InternalLink href={isActive ? `/archive` : href}>
      <a className={`btn mb-1 ${klass} ${isActive ? 'btn-tag-selected' : 'btn-tag-unselected'}`}>
        {children}
      </a>
    </InternalLink>
  )
}

TagLink.defaultProps = {
  klass: ''
}
