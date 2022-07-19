import { InternalLink } from '../components/internal-link'
import React from 'react'
import { useRouter } from 'next/router'

type Props = React.PropsWithChildren<{tag: string, klass: string}>

export const TagLink = ({ tag, klass, children }: Props) => {
  const router = useRouter()
  const href = `/tags/${tag.replaceAll(/\s/g, '+')}`
  const isActive = href === router.asPath.replaceAll('%2B', '+')
  const defaultKlass = [
    "tw-border-[1px]",
    "tw-border-solid",
    "tw-inline-flex",
    "tw-items-center",
    "tw-mb-2",
    "tw-mr-2",
    "tw-no-underline",
    "tw-px-3",
    "tw-py-2",
    "tw-rounded-md",
    "tw-text-md",
  ].join(" ")
  const activeKlass = [
    "hover:tw-bg-white",
    "hover:tw-border-[color:var(--blue)]",
    "hover:tw-fill-[color:var(--blue)]",
    "hover:tw-text-[color:var(--blue)]",
    "tw-bg-[color:var(--pink)]",
    "tw-border-[color:var(--pink)]",
    "tw-fill-white",
    "tw-text-white",
  ].join(" ")
  const inactiveKlass = [
    "hover:tw-bg-[color:var(--pink)]",
    "hover:tw-border-[color:var(--pink)]",
    "hover:tw-fill-white",
    "hover:tw-text-white",
    "tw-border-[color:var(--blue)]",
    "tw-fill-[color:var(--blue)]",
    "tw-text-[color:var(--blue)]",
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
