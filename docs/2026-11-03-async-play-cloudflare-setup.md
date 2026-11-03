# Async Play: Cloudflare Setup

- **Date:** 2026-11-03
- **For:** the room mailbox ([design](./2026-11-03-async-play-design.md) §6.4,
  [ticket](../issues/open/2026-11-03-async-play-mailbox.md))
- **Cost:** free. Everything below uses the Cloudflare **Workers Free** plan.

The mailbox is a Cloudflare Worker with one Durable Object per game room. To get it
running, you create a free Cloudflare account, pick a free `workers.dev` address, and give
GitHub two values so a GitHub Action can deploy the Worker. The game client needs no
credentials at all: it only needs the Worker's public URL.

## 1. What You Need

| Item | What it is | Secret? | Where it goes |
|---|---|---|---|
| Cloudflare account | A free account at [dash.cloudflare.com](https://dash.cloudflare.com/sign-up) | No | Nowhere |
| `workers.dev` subdomain | Your free address, `<subdomain>.workers.dev` | No | Chosen once in the dashboard |
| `CLOUDFLARE_ACCOUNT_ID` | A 32-character hex ID for your account | Not really, but keep it with the token | GitHub Actions secret |
| `CLOUDFLARE_API_TOKEN` | A token that may deploy Workers to your account | **Yes** | GitHub Actions secret |
| Mailbox URL | `https://domination-mailbox.<subdomain>.workers.dev`, known after the first deploy | No (public) | Sent to me, then compiled into the client |

You do **not** need:
- a domain name or DNS changes (`workers.dev` is enough),
- a credit card or a paid plan (Durable Objects with SQLite storage are on the free plan),
- KV, R2, D1 or any other Cloudflare product,
- any key in the browser. The mailbox is a public URL, like the relay.

## 2. Create the Account

1. Go to [dash.cloudflare.com/sign-up](https://dash.cloudflare.com/sign-up) and sign up
   with an email address and password.
2. Confirm the email Cloudflare sends you.
3. If Cloudflare asks what you want to do first, skip it or pick *Workers*. You do not need
   to add a website.

## 3. Choose the `workers.dev` Subdomain

The first deploy from GitHub Actions cannot answer questions, so pick the subdomain by hand
first.

1. In the dashboard, open **Workers & Pages** in the left sidebar.
2. Find **Subdomain** (on the overview page, in the account details on the right) and
   choose a name, for example `bagrounds`. Your Workers will then live at
   `https://<worker-name>.bagrounds.workers.dev`.
3. Save. It can take a few minutes before the subdomain answers.

## 4. Find the Account ID

Any of these works:
- In the dashboard, open **Workers & Pages**. **Account ID** is in the account details on
  the right, with a copy button.
- Look at the dashboard URL after you log in: `https://dash.cloudflare.com/<account-id>/...`.
  The long hex string is the account ID.
- If you have Node.js: run `npx wrangler login`, then `npx wrangler whoami`. It prints the
  account name and ID.

Copy it somewhere for step 6.

## 5. Create the API Token

The token lets GitHub Actions deploy the Worker. It should be able to do nothing else.

1. In the dashboard, open **Manage Account → Account API Tokens**, then **Create Token**.
   - If you do not see *Account API Tokens*, use your profile instead: the person icon at
     the top right → **My Profile → API Tokens → Create Token**. Either kind works.
2. Next to **Edit Cloudflare Workers**, click **Use template**. The template includes the
   permission the deploy needs, *Account → Workers Scripts → Edit*, which also covers
   Durable Objects.
3. Narrow it down:
   - **Account Resources:** *Include* → your account only.
   - **Zone Resources:** *Include* → *All zones from an account* → your account. You have
     no zones yet, so this grants nothing today.
   - Leave **Client IP Address Filtering** empty. GitHub's runners do not have fixed
     addresses.
   - **TTL:** optional. If you set an end date, deploys fail after it until you create a
     new token.
4. Click **Continue to summary**, then **Create Token**.
5. **Copy the token now.** Cloudflare shows it only once. If you lose it, roll it or
   create a new one; nothing else needs to change.

Do not paste the token into an issue, a commit, a chat message or the game's settings. It
belongs only in step 6.

## 6. Put Both Values in GitHub

The mailbox will be deployed by a GitHub Action in this repository (it will run only on
pushes to `master` that change `mailbox/`). GitHub keeps the values encrypted and hides
them in logs.

1. Open [the repository's Actions secrets](https://github.com/bagrounds/domination/settings/secrets/actions):
   **Settings → Secrets and variables → Actions**.
2. On the **Secrets** tab, click **New repository secret** and add:

   | Name (exactly) | Value |
   |---|---|
   | `CLOUDFLARE_API_TOKEN` | the token from step 5 |
   | `CLOUDFLARE_ACCOUNT_ID` | the account ID from step 4 |

3. That's all. You do not need to add anything to GitLab: production
   (`domination.fun`, GitLab Pages) only serves the client, and the client only needs the
   mailbox's public URL.

Secrets are not given to workflows triggered by pull requests from forks, so nobody
outside the repository can deploy with your token.

## 7. After the First Deploy

When the mailbox is built and merged, the deploy workflow prints the Worker's URL. It will
be:

```text
https://domination-mailbox.<your-subdomain>.workers.dev
```

1. Check it is alive. A room that has never been written to answers `204` with revision 0:

   ```sh
   curl -i https://domination-mailbox.<your-subdomain>.workers.dev/v1/rooms/test
   # HTTP/2 204
   # etag: "0"
   ```

2. Tell me the URL (or just the subdomain). I will make it the client's default mailbox,
   next to the default relay URL. It is public, like the relay's address.
3. Anyone can still point their own browser at a different mailbox, or turn async play
   off by clearing the field, in **Settings → Connection → Mailbox URL**.

## 8. Deploying or Testing by Hand (Optional)

You never need the API token on your own machine:
- **Local testing:** `cd mailbox && npx wrangler dev` runs the Worker and Durable Objects
  on your computer with local storage. It needs no account.
- **Manual deploy:** `npx wrangler login` opens a browser to authorize Wrangler with your
  Cloudflare login. Then `cd mailbox && npx wrangler deploy`.

## 9. Keeping It Safe and Free

- **If the token leaks:** open the API tokens page from step 5 and **Roll** or **Delete**
  it, then update `CLOUDFLARE_API_TOKEN` in GitHub. The token can only deploy Workers. It
  cannot read your email, billing or other accounts.
- **Limits:** the free plan allows 100,000 requests a day, which is about 2,000 short games.
  When a free limit is reached, Cloudflare returns errors until the next day. It does not
  start billing. The game then falls back to live play only (design §8).
- **Usage:** **Workers & Pages → domination-mailbox → Metrics** shows requests and errors.

## 10. Checklist

- [ ] Cloudflare account created and email confirmed
- [ ] `workers.dev` subdomain chosen
- [ ] Account ID copied
- [ ] API token created from the *Edit Cloudflare Workers* template, limited to your account
- [ ] `CLOUDFLARE_API_TOKEN` and `CLOUDFLARE_ACCOUNT_ID` added as GitHub Actions repository
      secrets
- [ ] (After the mailbox ships) the Worker URL answers `204` and has been sent to me
