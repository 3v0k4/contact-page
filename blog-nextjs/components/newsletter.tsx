import Link from "next/link";

const Newsletter = () => (
  <div
    id="newsletter"
    className="shadow-[0_0_5px_var(--pink)] text-white bg-[color:var(--pink)] text-center py-20 px-4"
  >
    <div className="flex flex-wrap-reverse place-items-center mx-auto gap-12 justify-around max-w-7xl">
      <div className="flex-1 basis-[350px]">
        <h2 className="text-4xl font-semibold mb-4">PinkLetter</h2>

        <form
          action="https://app.convertkit.com/forms/3877254/subscriptions"
          method="post"
          className="mx-auto max-w-[400px]"
          target="popupwindow"
        >
          <input
            className="w-[100%] px-3 py-2 rounded-md text-black"
            type="email"
            name="email_address"
            placeholder="Email address"
            required
          />
          <input type="hidden" value="1" name="embed" />
          <input
            className="whitespace-normal w-[100%] mt-4 bg-[color:var(--blue)] hover:bg-[color:var(--dark-blue)] px-3 py-2 rounded-md cursor-pointer disabled:bg-gray-500 disabled:cursor-not-allowed"
            type="submit"
            value="Closed for vacation"
            disabled
          ></input>
        </form>

        <p className="text-sm mt-4">
          <span className="italic">
            It&apos;s one of the selected few I follow every week
          </span>{" "}
          – Mateusz
        </p>
      </div>

      <div className="flex-1 basis-[350px] text-center">
        <div className="text-left inline-block">
          <h2 className="text-4xl mb-4">Tired of RELEARNING webdev stuff?</h2>

          <div className="">
            <ul className="text-left list-[circle] list-inside">
              <li>
                A 100+ page book with the best links I curated over the years
              </li>
              <li className="mt-1">
                An email once a week full of timeless software wisdom
              </li>
              <li className="mt-1">Your recommended weekly dose of pink</li>
              <li className="mt-1">
                <span className="font-bold">Try before you buy? </span>
                <Link href="https://buttondown.email/riccardo.odone/archive">
                  <a
                    className="underline hover:text-white"
                    target="_blank"
                    rel="noopener"
                  >
                    Check the archives
                  </a>
                </Link>
                .
              </li>
            </ul>
          </div>
        </div>
      </div>
    </div>
  </div>
);

export default Newsletter;
