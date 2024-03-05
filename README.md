# Blob

Store shareable text blobs.

## Setup

```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
APP_TITLE="My App" REDIRECT_URL=https://google.com gunicorn server:app
```