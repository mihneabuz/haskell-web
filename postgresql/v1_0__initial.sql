create extension citext;

create table users (
  id bigserial primary key not null,
  name citext not null unique
);
