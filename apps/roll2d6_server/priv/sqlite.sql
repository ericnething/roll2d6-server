-- Roll2d6 Virtual Tabletop Project
--
-- Copyright (C) 2018-2019 Eric Nething <eric@roll2d6.org>
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU Affero General Public License
-- as published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public
-- License along with this program. If not, see
-- <https://www.gnu.org/licenses/>.

PRAGMA foreign_keys = ON;
PRAGMA journal_mode = WAL;
--PRAGMA auto_vacuum = INCREMENTAL;

------------------------------------------------------------
-- Person
------------------------------------------------------------

create table person (
  id         varchar      not null,
  username   varchar      not null unique,
  email      varchar      not null unique,
  password   varchar      not null,
  active     boolean      not null default true,
  created_at datetime     not null default current_timestamp,
  primary key (id)
);

------------------------------------------------------------
-- Games
------------------------------------------------------------

create table game (
  id         varchar      not null,
  title      varchar      not null,
  game_type  varchar      not null,
  created_at datetime     not null default current_timestamp,
  primary key (id)
);

------------------------------------------------------------
-- Person Game Relations
------------------------------------------------------------

create table person_game_relation (
  game_id    varchar      not null references game (id),
  person_id  varchar      not null references person (id),
  access     varchar      not null,
  created_at datetime     not null default current_timestamp,
  primary key (game_id, person_id)
);
