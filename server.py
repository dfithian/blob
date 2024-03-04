from datetime import datetime
from flask import Flask, abort, jsonify, render_template, request
import os
import psycopg2

app = Flask(__name__)

APP_TITLE = os.environ.get('APP_TITLE')
REDIRECT_URL = os.environ.get('REDIRECT_URL')
DATABASE_URL = os.environ.get('DATABASE_URL', 'postgres://localhost:5432/postgres')

@app.route('/blob/<blob_id>', methods=['GET', 'PUT'])
def put_blob(blob_id):
    pin = request.headers.get('X-Blob-Pin')
    if pin is None:
        abort(404)
    if request.method == 'GET':
        conn = psycopg2.connect(DATABASE_URL)
        with conn:
            with conn.cursor() as cur:
                cur.execute('SELECT blob_pin, blob, modified_at FROM blob.blobs WHERE blob_id = %s', (blob_id,))
                row = cur.fetchone()
                if row is not None:
                    abort(404)
                if pin != row[0]:
                    abort(404)
                return jsonify({
                    'blob': row[1],
                    'modifiedAt': row[2].isoformat(timespec='seconds')
                })
    elif request.method == 'PUT':
        blob = request.data.decode('utf-8')
        conn = psycopg2.connect(DATABASE_URL)
        with conn:
            with conn.cursor() as cur:
                cur.execute('SELECT blob_pin FROM blob.blobs WHERE blob_id = %s', (blob_id,))
                row = cur.fetchone()
                if row is not None:
                    if pin == row[0]:
                        cur.execute('UPDATE blob.blobs SET blob = %s, modified_at = NOW() WHERE blob_id = %s', (blob, blob_id))
                        return jsonify({})
                    else:
                        abort(404)
                else:
                    cur.execute('INSERT INTO blob.blobs (blob_id, blob_pin, blob, modified_at) VALUES (%s, %s, %s, NOW())', (blob_id, pin, blob))
                    return jsonify({})
    else:
        abort(404)

@app.errorhandler(404)
def page_not_found(e):
    return render_template('404.html', app_title=APP_TITLE, redirect_url=REDIRECT_URL), 404

if __name__ == '__main__':
    app.run(debug=True)
