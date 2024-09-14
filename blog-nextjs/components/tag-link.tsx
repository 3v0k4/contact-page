import { InternalLink } from '../components/internal-link'
import React from 'react'
import { useRouter } from 'next/router'

type Props = React.PropsWithChildren<{href: string, klass?: string}>

export const TagLink = ({ href, klass, children }: Props) => {
  const router = useRouter()
  const isActive = href === router.asPath
  const defaultKlass = [
    "border-[1px]",
    "border-solid",
    "inline-flex",
    "items-center",
    "no-underline",
    "px-3",
    "py-2",
    "rounded-md",
    "text-md",
  ].join(" ")
  const activeKlass = [
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
    "border-gray-500",
    "fill-gray-500",
    "text-gray-500",
  ].join(" ")

  return (
    <InternalLink href={href}>
      <a className={`${defaultKlass} ${klass} ${isActive ? activeKlass : inactiveKlass}`}>
        {children}
      </a>
    </InternalLink>
  )
}
