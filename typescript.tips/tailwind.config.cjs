/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ['./src/**/*.{astro,html,js,jsx,md,mdx,svelte,ts,tsx,vue}'],
  theme: {
    extend: {
      colors: {
        blue: '#3178C6',
        yellow: '#F1DD35',
        darkYellow: '#C6BF31',
      }
    }
  },
  plugins: [],
}
