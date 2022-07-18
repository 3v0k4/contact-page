import Link from 'next/link'

const Newsletter = () => (
  <div id="newsletter" className="newsletter-container tw-text-center py-5 mt-5">
    <div className="col-12 col-sm-12 col-md-6 col-xl-6 mx-auto mt-sm-50">
      <h2>PinkLetter</h2>

      <form
        action="https://buttondown.email/api/emails/embed-subscribe/riccardo.odone"
        method="post"
        target="popupwindow"
        onSubmit={() => window.open('https://buttondown.email/riccardo.odone', 'popupwindow')}
        className="my-4"
        style={{ margin: 'auto', maxWidth: '400px', width: '80%' }}
      >
        <input className="input-lg form-control fs-bigger" type="email" name="email" id="bd-email" placeholder="Email address" required />
        <input type="hidden" value="1" name="embed" />

        <div className="mt-4 tw-text-left">
          <input className="btn btn-primary fs-bigger w-100 white-space-normal" type="submit" value="Send me evergreen software wisdom!"></input>
        </div>
      </form>

      <p className="small mt-4">
        <span style={{ fontStyle: 'italic' }}>It&apos;s one of the selected few I follow every week</span> – Mateusz
      </p>
    </div>

    <div className="col-12 col-sm-12 col-md-6 col-xl-6 mx-auto">
      <h2>Tired of RELEARNING webdev stuff?</h2>

      <div className="mt-25 w-80 mx-auto">
        <ul className="tw-text-left list-style-circle list-style-inside p-0 m-0">
          <li>A 79-page book with the best links I curated over the years</li>
          <li className="mt-5-">An email once a week full of timeless software wisdom</li>
          <li className="mt-5-">Your recommended weekly dose of pink</li>
          <li className="mt-5-"><span className="tw-font-bold">Try before you buy? </span><Link href="https://buttondown.email/riccardo.odone/archive"><a target="_blank" rel="noopener">Check the archives</a></Link>.</li>
        </ul>
      </div>
    </div>
  </div>
)

export default Newsletter
