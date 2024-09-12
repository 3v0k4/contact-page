/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./pages/**/*.{js,ts,jsx,tsx}",
    "./components/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      colors: {
        'blue-diabetes': "#578ad6",
        'pinkk': "#f76ca5",
        'blue-dark': "#0070e8",
      },
    },
  },
  plugins: [],
}
