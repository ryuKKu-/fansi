# Changelog

## 0.1.0 (2026-10-10)


### Features

* add a focus ring and an interactive sample ([e89cc8a](https://github.com/ryuKKu-/fansi/commit/e89cc8a5a8c411087ca43a6949869ced4f463a5e))
* add a help component drawn from key bindings ([78e1fb2](https://github.com/ryuKKu-/fansi/commit/78e1fb205ca992fc65ea7b9f75dffeccdf43bfa3))
* add a scrolling viewport component ([0382341](https://github.com/ryuKKu-/fansi/commit/0382341cf48e7322ad92d079b058c77e4b6f9173))
* add a table component with a cursor row ([b075688](https://github.com/ryuKKu-/fansi/commit/b075688ecbd66b7d5879a4f537cf215c6a06448a))
* add a table sample with a help line ([077de53](https://github.com/ryuKKu-/fansi/commit/077de53d5f72132f4e9aaedbcd5ff7ab8e20d6e5))
* add directions for List component ([f1609a5](https://github.com/ryuKKu-/fansi/commit/f1609a52ae59c40185cbbaca00e95137ba7abd7a))
* add Sub and Cursor modules and bind keys on KeyEvent ([bc59587](https://github.com/ryuKKu-/fansi/commit/bc59587b2f587a03516f42a85da273b77abef379))
* add the layout and dashboard samples ([86a9fe3](https://github.com/ryuKKu-/fansi/commit/86a9fe33e1f8bec3b97094aafaee3567016028d1))
* count text input characters as glyphs and terminal cells ([ee05403](https://github.com/ryuKKu-/fansi/commit/ee05403de8b68bb48dd395e6db65197ea7182830))
* draw a configurable grid border around tables ([b7f2d46](https://github.com/ryuKKu-/fansi/commit/b7f2d465749211868a4cdd1cf11f945482fe11df))
* draw a first layout in the terminal ([23ce7b6](https://github.com/ryuKKu-/fansi/commit/23ce7b631874bc82838c17286179a6afabb94145))
* lay out views with integer constraints and paint them into a cell buffer ([380a882](https://github.com/ryuKKu-/fansi/commit/380a88206f55dbe35193901b65055339edd044fc))
* measure and wrap text in terminal cells, with styled lines and border titles ([bdd4928](https://github.com/ryuKKu-/fansi/commit/bdd4928d181726f003a96c778160e5a67b01a322))
* package the library as Fansi.Tui under the MIT licence ([c6a30c3](https://github.com/ryuKKu-/fansi/commit/c6a30c3065eb19fd3a0eff81411e8f9c60e5cb3f))
* read raw keys, mouse and paste on their own thread ([2e0cfed](https://github.com/ryuKKu-/fansi/commit/2e0cfedb45122bd4b6427f89d5f9134b048812c4))
* rewrite TextInput with scrolling, undo, validation and suggestions ([5dc4d84](https://github.com/ryuKKu-/fansi/commit/5dc4d8455d64fdefd8914af471cadfa689649ef7))


### Bug Fixes

* **ci:** dotnet sdk version ([2d0d8bd](https://github.com/ryuKKu-/fansi/commit/2d0d8bd727a0464ee9c8de054b33c623023aba74))
* drop timer ticks that arrive after the subscription is disposed ([317acc8](https://github.com/ryuKKu-/fansi/commit/317acc8cf9070f140df25492873267a52b124b9d))
* harden the layout arithmetic and the renderer ([c280e12](https://github.com/ryuKKu-/fansi/commit/c280e127d920e08c9351facbaec088133a473de6))
* remove custom label for timer component ([403820f](https://github.com/ryuKKu-/fansi/commit/403820f7d778f86f64a9712a77fdf37455f7a388))
* **table:** draw only outer borders and highlight only the row content ([32dea7e](https://github.com/ryuKKu-/fansi/commit/32dea7ea5e11f1616b26e4cffa4d9facbb2b61a5))
