# matrix api notes

## Token

Create an access token:

```ShellSession
HOMESERVER_URL="https://matrix.org"
USER="@tristanc_:matrix.org"

PASS="supersecret"
export MATRIX_TOKEN=$(curl -XPOST ${HOMESERVER_URL}/_matrix/client/r0/login -d '{"user": "'${USER}'", "password": "'${PASS}'", "type": "m.login.password"}' | jq -r ".access_token")
```

You can also get the `MATRIX_TOKEN` from element by visiting the account `Settings` page, `Help & About` panel, then click `Access Token`.

Verity the token:

```ShellSession
curl -H "Authorization: Bearer ${MATRIX_TOKEN}" ${HOMESERVER_URL}/_matrix/client/r0/account/whoami
```

## Identity token

Create an identity access token:

```ShellSession
MATRIX_OPENID=$(curl -XPOST ${HOMESERVER_URL}/_matrix/client/r0/user/${USER}/openid/request_token -H "Authorization: Bearer ${MATRIX_TOKEN}" -d '{}')
IDENTITY_URL="https://matrix.org"

export MATRIX_IDENTITY_TOKEN=$(curl -XPOST ${IDENTITY_URL}/_matrix/identity/v2/account/register -d "${MATRIX_OPENID}" | jq -r '.access_token')
```

You might need to accept terms:

```ShellSession
curl -H "Authorization: Bearer ${MATRIX_IDENTITY_TOKEN}" ${IDENTITY_URL}/_matrix/identity/v2/terms
```

```json
{
  "policies": {
    "privacy_notice": {
      "version": "1",
      "en": {
        "url": "https://matrix.org/legal/identity-server-privacy-notice-1",
        "name": "Privacy Notice"
      }
    }
  }
}
```

Then accept it:

```ShellSession
curl -XPOST ${IDENTITY_URL}/_matrix/identity/v2/terms -H "Authorization: Bearer ${MATRIX_IDENTITY_TOKEN}" -d '{"user_accepts": ["https://matrix.org/legal/identity-server-privacy-notice-1"]}'
```

```json
{}
```

Verify the token:

```ShellSession
curl -H "Authorization: Bearer ${MATRIX_IDENTITY_TOKEN}" ${IDENTITY_URL}/_matrix/identity/v2/account
```

## Hash pepper

To resolve identity, get the pepper:

```ShellSession
export MATRIX_IDENTITY_PEPPER=$(curl -H "Authorization: Bearer ${MATRIX_IDENTITY_TOKEN}" ${IDENTITY_URL}/_matrix/identity/v2/hash_details | jq -r '.lookup_pepper')
```


## Associate your gerrit author email to a matrix account

Check your existing associated account:

```ShellSession
curl -H "Authorization: Bearer ${MATRIX_TOKEN}" ${HOMESERVER_URL}/_matrix/client/r0/account/3pid
```

The transaction will requires a client secret:

```ShellSession
CLIENT_SECRET=$(uuidgen)
```

First you need to verify your email:

```ShellSession
curl -XPOST ${IDENTITY_URL}/_matrix/identity/v2/validate/email/requestToken \
  -H "Authorization: Bearer ${MATRIX_IDENTITY_TOKEN}"                       \
  -H "Content-Type: application/json"                                       \
  -d '{"client_secret": "'${CLIENT_SECRET}'", "email": "YOUR_EMAIL", "send_attempt": 1}'
```

```json
{"sid": "SESSION_NUMBER"}
```

Store that session id as well:

```ShellSession
SID=SESSION_NUMBER
```

Once you receive the token by mail, submit it:

```ShellSession
curl -XPOST ${IDENTITY_URL}/_matrix/identity/v2/validate/email/submitToken  \
  -H "Authorization: Bearer ${MATRIX_IDENTITY_TOKEN}"                       \
  -H "Content-Type: application/json"                                       \
  -d '{"client_secret": "'${CLIENT_SECRET}'", "sid": "'${SID}'", "token": "TOKEN_RECEIVED_BY_MAIL"}'
```

```json
{"success": true}
```

Then you can bind the identity email to your matrix account:

```ShellSession
curl -XPOST ${HOMESERVER_URL}/_matrix/client/r0/account/3pid/bind         \
  -H "Authorization: Bearer ${MATRIX_TOKEN}"                              \
  -H "Content-Type: application/json"                                     \
  -d '{"client_secret": "'${CLIENT_SECRET}'", "sid": "'${SID}'", "id_server": "matrix.org", "id_access_token": "'${MATRIX_IDENTITY_TOKEN}'"}'
```

Check the bind is registered, if the output is empty, make sure your home server is configured with `enable_3pid_lookup: true`:

```ShellSession
curl -H "Authorization: Bearer ${MATRIX_TOKEN}" ${HOMESERVER_URL}/_matrix/client/r0/account/3pid
```

To summarize, in order to associate your gerrit account email to your matrix accound id, you need to:

- Get a matrix token (`/_matrix/client/r0/login`)
- Get a matrix openid token (`/_matrix/client/r0/user/${USER}/openid/request_token`)
- Get a identity token (`/_matrix/identity/v2/account/register`)
- Accept the identity terms (`/_matrix/identity/v2/terms`)
- Validate email using two requests (`/_matrix/identity/v2/validate/email/`)
- Bind the identity 3pid (`/_matrix/client/r0/account/3pid/bind`)

If you want to verify your 3pid, you can do a lookup against the identity service. The email needs to be quoted:

```ShellSession
curl "${IDENTITY_URL}/_matrix/identity/api/v1/lookup?medium=email&address=user%40example.com"
