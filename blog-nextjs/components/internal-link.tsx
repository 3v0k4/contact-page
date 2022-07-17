import Link from 'next/link'
import React from 'react'

const extension = (href: string) => {
  if (href === '/') return ''
  if (process.env.NODE_ENV !== 'production') return ''
  return '.html'
}

export const InternalLink = ({ href, children }: React.PropsWithChildren<{ href: string }>) => {
  return <Link href={`${href}${extension(href)}`}>{children}</Link>
}
