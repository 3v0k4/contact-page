import Link from 'next/link'

const Newsletter = () => (
  <div id="newsletter" className="tw-shadow-[0_0_5px_var(--pink)] tw-text-white tw-bg-[color:var(--pink)] tw-text-center tw-py-20 tw-px-4">
    <div className="tw-flex tw-flex-wrap-reverse tw-place-items-center tw-mx-auto tw-gap-12 tw-justify-around">
      <div className="tw-flex-1 tw-min-w-[350px]">
        <h2 className="tw-text-4xl tw-font-semibold tw-mb-4">PinkLetter</h2>

        <form
          action="https://buttondown.email/api/emails/embed-subscribe/riccardo.odone"
          method="post"
          target="popupwindow"
          onSubmit={() => window.open('https://buttondown.email/riccardo.odone', 'popupwindow')}
          className="tw-mx-auto tw-max-w-[400px]"
        >
          <input className="tw-w-[100%] tw-px-3 tw-py-2 tw-rounded-md" type="email" name="email" id="bd-email" placeholder="Email address" required />
          <input type="hidden" value="1" name="embed" />
          <input className="tw-whitespace-normal tw-w-[100%] tw-mt-4 tw-bg-[color:var(--blue)] hover:tw-bg-[color:var(--dark-blue)] tw-px-3 tw-py-2 tw-rounded-md tw-cursor-pointer" type="submit" value="Send me evergreen software wisdom!"></input>
        </form>

        <p className="tw-text-sm tw-mt-4">
          <span className="tw-italic">It&apos;s one of the selected few I follow every week</span> – Mateusz
        </p>
      </div>

      <div className="tw-flex-1 tw-min-w-[350px] tw-text-center">
        <div className="tw-text-left tw-inline-block">
          <h2 className="tw-text-4xl tw-mb-4">Tired of RELEARNING webdev stuff?</h2>

          <div className="">
            <ul className="tw-text-left tw-list-[circle] tw-list-inside">
              <li>A 79-page book with the best links I curated over the years</li>
              <li className="tw-mt-1">An email once a week full of timeless software wisdom</li>
              <li className="tw-mt-1">Your recommended weekly dose of pink</li>
              <li className="tw-mt-1"><span className="tw-font-bold">Try before you buy? </span>
                <Link href="https://buttondown.email/riccardo.odone/archive">
                  <a className="tw-underline hover:tw-text-white" target="_blank" rel="noopener">Check the archives</a>
                </Link>.
              </li>
            </ul>
          </div>
        </div>
      </div>
    </div>
  </div>
)

export default Newsletter
