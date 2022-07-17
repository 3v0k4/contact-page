import { InternalLink } from '../components/internal-link'

const Custom404 = () => (
  <div className="col-sm-8 col-md-8 col-lg-6 mx-auto mt-4">
    <h1 id="page-not-found">Page Not Found</h1>

    <p>You may want to visit the <InternalLink href="/"><a>homepage</a></InternalLink>. Or keep reading for my apologies.</p>

    <figure>
      <img src="/images/404.png" alt="Scan of a hand-drawn sketch representing the number 404 with two hands counting four and a bug in the middle" />
      <figcaption aria-hidden="true">Scan of a hand-drawn sketch representing the number 404 with two hands counting four and a bug in the middle</figcaption>
    </figure>

    <p>I am sorry.</p>

    <p>If you got here by clicking on a link and you are not an internet bot, then I made a mistake.</p>

    <p>I could call it a bug to protect my professional self worth. But I want to take ownership instead. Blaming an insect roaming around the codebase is unprofessional: it’s my job to put the most care in my craft.</p>

    <p>Using the b-word doesn’t build trust. Taking responsibility, asking for forgiveness and doing better does.</p>

    <p>I make mistakes all the time. However, I hope you will be so kind to let me know when I do. I want to hone my skills and your contribution is paramount.</p>

    <blockquote>
      <p>“A mistake is a future benefit, the full value of which is yet to be realized.”</p>
      <p>–Edwin Land</p>
    </blockquote>
  </div>
)

export default Custom404
