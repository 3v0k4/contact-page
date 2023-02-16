import Link from 'next/link'
import React from 'react'

export const InternalLink = ({ href, children }: React.PropsWithChildren<{ href: string }>) => {
  return <Link href={href}>{children}</Link>
}
