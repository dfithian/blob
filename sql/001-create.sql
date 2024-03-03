create schema blob;

create table blob.blobs (
    blob_id text primary key,
    blob_pin text not null,
    blob text not null,
    modified_at timestamptz not null
);
