import Link from 'next/link'
import React from 'react'
import { useRouter } from 'next/router'

type Props = React.PropsWithChildren<{tag: string, klass: string}>

export const TagLink = ({ tag, klass, children }: Props) => {
  const router = useRouter()
  const href = `/tags/${tag.replaceAll(/\s/g, '+')}`
  const hrefExt = `${href}${process.env.NODE_ENV === 'production' ? '.html' : ''}`
  const isActive = [href, hrefExt].includes(router.asPath.replaceAll('%2B', '+'))

  return (
    <Link href={isActive ? `/archive${process.env.NODE_ENV === 'production' ? '.html' : ''}` : hrefExt}>
      <a className={`btn mb-1 ${klass} ${isActive ? 'btn-tag-selected' : 'btn-tag-unselected'}`}>
        {children}
      </a>
    </Link>
  )
}

TagLink.defaultProps = {
  klass: ''
}
