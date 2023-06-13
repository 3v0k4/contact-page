import { InternalLink } from '../components/internal-link'
import React from 'react'
import { useRouter } from 'next/router'

type Props = React.PropsWithChildren<{tag: string, klass: string}>

export const TagLink = ({ tag, klass, children }: Props) => {
  const router = useRouter()
  const href = `/tags/${tag}/`.replace(" ", "%20")
  const isActive = href === router.asPath
  const defaultKlass = [
    "border-[1px]",
    "border-solid",
    "inline-flex",
    "items-center",
    "mb-2",
    "mr-2",
    "no-underline",
    "px-3",
    "py-2",
    "rounded-md",
    "text-md",
  ].join(" ")
  const activeKlass = [
    "hover:bg-white",
    "hover:border-[color:var(--blue)]",
    "hover:fill-[color:var(--blue)]",
    "hover:text-[color:var(--blue)]",
    "bg-[color:var(--pink)]",
    "border-[color:var(--pink)]",
    "fill-white",
    "text-white",
  ].join(" ")
  const inactiveKlass = [
    "hover:bg-[color:var(--pink)]",
    "hover:border-[color:var(--pink)]",
    "hover:fill-white",
    "hover:text-white",
    "border-[color:var(--blue)]",
    "fill-[color:var(--blue)]",
    "text-[color:var(--blue)]",
  ].join(" ")

  return (
    <InternalLink href={isActive ? `/archive` : href}>
      <a className={`${defaultKlass} ${klass} ${isActive ? activeKlass : inactiveKlass}`}>
        {children}
      </a>
    </InternalLink>
  )
}

TagLink.defaultProps = {
  klass: ''
}
