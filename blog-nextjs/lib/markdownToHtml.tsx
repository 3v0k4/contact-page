import { unified } from "unified";
import remarkParse from "remark-parse";
import remarkRehype from "remark-rehype";
import rehypeHighlight from 'rehype-highlight'
import rehypeStringify from "rehype-stringify";
import remarkSupersub from 'remark-supersub'
import rehypeRaw from 'rehype-raw'
import { all } from 'lowlight'

const markdownToHtml = async (markdown: string) => {
  const result = await unified()
    .use(remarkParse)
    .use(remarkSupersub)
    .use(remarkRehype, { allowDangerousHtml: true })
    .use(rehypeRaw)
    .use(rehypeHighlight, { detect: true, languages: all })
    .use(rehypeStringify)
    .process(markdown)

  return result.toString()
}

export default markdownToHtml
