create table users (
  id bigserial primary key not null,
  username text not null unique,
  mail text not null,
  password text not null
);
