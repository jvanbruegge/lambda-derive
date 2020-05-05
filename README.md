# lambda-derive

A demonstration that running Haskell on Lambda is not that hard and might be useful for some projects.

## Deploy this yourself

You will need a Telegram Bot Token, [see here](https://core.telegram.org/bots#6-botfather) how to get one. Create a file name `vars.tfvars` in the `terraform/` folder with this content:

```
telegram_bot_token = "bot<TOKEN>"
```

Then it's just a matter of running `make && make deploy`.

As a last step you have to tell telegram where to find your bot:

```bash
export BOT_TOKEN=bot<TOKEN>
export BOT_URL=$(cd terraform && terraform output -json | jq -r '.base_url.value')
curl -XPOST \
    --header 'content-type: application/json' \
    --data "{ \"url\": \"$BOT_URL/$BOT_TOKEN\" }" \
    --url "https://api.telegram.org/$BOT_TOKEN/setWebhook"
```
