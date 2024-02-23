import asyncio
import os
import sqlite3
from dataclasses import dataclass
from datetime import datetime
from random import choice

import aiohttp
from aiogram import Bot, types
from aiogram import Dispatcher
from aiogram.enums import ParseMode
from aiogram.filters import CommandStart, Command
from aiogram.utils.keyboard import InlineKeyboardBuilder
from aiogram.utils.markdown import hbold, hitalic, hlink
from bs4 import BeautifulSoup

dp = Dispatcher()

stickers = [
    'CAACAgIAAxkBAAN2ZYjr_LzWUIPHlbtntV3X02wJV7EAAv8MAAKHtzhLq3LtW8R6qnUzBA',
    'CAACAgIAAxkBAAN5ZYjsdZwOD7fqtn0JewZvn9mOJskAAgYLAAJriKBLwI62PCDkZdIzBA',
    'CAACAgIAAxkBAAN6ZYjsnDqruEq0mcDa5nPqA1jgw5IAAkIBAAIw1J0RuA3l-BMlJcAzBA',
    'CAACAgIAAxkBAAN7ZYjswAm6KhdmZ1mF1lR_lPcNkrMAAgYBAAJWnb0KVOvwk33zluszBA',
    'CAACAgIAAxkBAAN8ZYjs9aM9IIxxF23slLGz4oPWRE0AAhIVAAJeKzlIRv6Zw8gVXpIzBA',
    'CAACAgIAAxkBAAN9ZYjtEls1wHIDKGorYaM5Sz33n9UAAsgOAAKfXulJo0vpcxpyfVczBA',
    'CAACAgIAAxkBAAN-ZYjtQMzLb1vILhrakEl-p7v7uygAAnA6AAI2KQFIlE7bK16jjx4zBA'
]

WELCOME_MSG_ = """
привет я синема бот
по твоему запросу я могу найти нужный тебе фильм вместе с описанием, актерами и ссылкой на ЗАКОННЫЙ просмотр
просто отправь мне название фильма, например, "таксидермия", и все будет очень хорошо
"""

COMMANDS_ = """
/start - начало работы
/stats - статистика предложенных вам фильмов
/history - история последних запросов
"""

film_prefix = "https://www.kinopoisk.ru/film/"
series_prefix = "https://www.kinopoisk.ru/series/"

API_HEADER = {'X-API-KEY': os.getenv("KINOPOISK_API_KEY")}
kinopoisk_api = "https://api.kinopoisk.dev/v1.4/movie/"

PLACEHOLDER_POSTER = "https://pm1.aminoapps.com/7261/9a5ff76ae20acc6ab4166c4c808dd07e8f53df65r1-1022-1024v2_uhq.jpg"


@dataclass
class MovieInfo:
    movie_id: str
    movie_url: str
    name: str
    poster_url: str | None
    year: str
    genres: list[str]
    director: str | None
    imdb_rating: float
    description: str
    watch_url: str
    trailer_url: str | None
    dt: datetime
    count: int

    def __str__(self) -> str:
        description = self.description if self.description is not None else "что-то там"
        return f"""
{hbold("НАЗВАНИЕ")}: {self.name}

{hbold("РЕЖИССЕР")}: {self.director if self.director is not None else "кто-то там"}

{hbold("ГОД")}: {self.year}

{hbold("ЖАНРЫ")}: {', '.join(self.genres)}

{hbold("РЕЙТИНГ IMDB")}: {self.imdb_rating}

{hbold("ОПИСАНИЕ")}: {hitalic(description)
        if len(description) <= 900 else f"не влезает, вот {hlink('ссылка', self.movie_url)}"}
"""


@dp.message(CommandStart())
async def send_welcome(message: types.Message) -> None:
    await message.reply(WELCOME_MSG_)


@dp.message(Command("help"))
async def list_commands(message: types.Message) -> None:
    await message.reply(COMMANDS_)


@dp.message(Command("history"))
async def gather_history(message: types.Message) -> None:
    history_query = f"""
    SELECT name FROM {get_username(message)} ORDER BY dt
    """
    with con:
        cursor = con.cursor()
        cursor.execute(history_query, ())
        history = '\n'.join(row[0] for row in cursor.fetchall()[-10:])
        if history:
            await message.reply("история ваших запросов выдает в вас законченного кретина: \n\n" + history)
        else:
            await message.reply("тут пока пусто попроси у меня кино пожалуйста")


@dp.message(Command("stats"))
async def gather_stats(message: types.Message) -> None:
    stats_query = f"""
    SELECT name, count FROM {get_username(message)} ORDER BY count
    """
    with con:
        cursor = con.cursor()
        cursor.execute(stats_query, ())
        stats = '\n'.join(f"{row[0]}: {row[1]}" for row in reversed(cursor.fetchall()))
        if stats:
            await message.reply("что это такое?? ах да это же статистика предложенных вам фильмов!)) \n\n" + stats)
        else:
            await message.reply("начни искать фильмы мб")


@dp.message()
async def find_movie(message: types.Message) -> None:
    if message.text is None:
        await message.reply_sticker(choice(stickers))
        return
    query = message.text.replace(' ', '+') + '+кинопоиск'
    res = await get_google_search_results(query)
    if res is not None:
        username = get_username(message)
        info = await fetch_info(res, username)
        keyboard = InlineKeyboardBuilder()
        keyboard.button(text="СМОТРЕТЬ", url=info.watch_url)
        if info.trailer_url is not None:
            keyboard.button(text="ТРЕЙЛЕР", url=info.trailer_url)
        poster = info.poster_url if info.poster_url is not None else PLACEHOLDER_POSTER
        await message.reply_photo(
            photo=poster,
            caption=str(info),
            reply_markup=keyboard.as_markup()
        )
    else:
        await message.reply(f"ничего не нашлось по запросу {message.text}, подумайте над своим поведением")


async def get_google_search_results(query: str) -> str | None:
    url = f"https://www.google.com/search?q={query}"
    custom_headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 11.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) '
                                    'Chrome/58.0.3029.110 Safari/537.3'}
    async with aiohttp.ClientSession() as session:
        async with session.get(url, headers=custom_headers) as response:
            html = await response.text()
            soup = BeautifulSoup(html, 'html.parser')
            result = soup.find('div', {'class': 'egMi0 kCrYT'})
            raw_link = result.find('a', href=True).get('href')
            link = extract_link(raw_link)
            if link.startswith(film_prefix) or link.startswith(series_prefix):
                return link
            else:
                return None


async def fetch_info(link: str, username: str) -> MovieInfo:
    movie_id = extract_id(link)
    from_db = fetch_from_db(movie_id, username)
    if from_db is not None:
        print('from db')
        return from_db
    url = kinopoisk_api + movie_id
    async with aiohttp.ClientSession() as session:
        async with session.get(url, headers=API_HEADER) as response:
            json = await response.json()
            trailer = None
            if 'videos' in json and json['videos'] is not None and (trailers := json['videos']['trailers']):
                trailer = trailers[0]['url']
            director_name = None
            directors = list(filter(lambda p: p['enProfession'] == "director", json['persons']))
            if directors:
                director_name = directors[0]['name']
            if 'shortDescription' in json and (short := json['shortDescription']) is not None:
                description = short
            else:
                description = json['description']
            movie = MovieInfo(
                movie_id=movie_id,
                movie_url=link,
                name=json['name'],
                poster_url=json['poster']['url'],
                year=json['year'],
                genres=[g['name'] for g in json['genres']],
                director=director_name,
                imdb_rating=float(json['rating']['imdb']),
                description=description,
                watch_url=link.replace('.ru', '.gg'),
                trailer_url=trailer,
                dt=datetime.now(),
                count=1
            )
            add_movie_to_db(movie, username)
            return movie


def extract_link(link: str) -> str:
    return link.split("&url=")[1].split("&ved=")[0]


def extract_id(link: str) -> str:
    if 'film' in link:
        drop_prefix = link.replace(film_prefix, '')
    else:
        drop_prefix = link.replace(series_prefix, '')
    return drop_prefix.split('/')[0]


def add_movie_to_db(movie: MovieInfo, username: str) -> None:
    create_table_query = f"""
    CREATE TABLE IF NOT EXISTS {username}
    (movie_id INTEGER, movie_url TEXT, name TEXT, poster_url TEXT, year TEXT, genres TEXT, director TEXT,
    imdb_rating REAL, description TEXT, watch_url TEXT, trailer_url TEXT, dt NUMERIC, count INTEGER)
    """
    insert_query = f"""
    INSERT INTO {username} VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    """
    with con:
        cursor = con.cursor()
        cursor.execute(create_table_query, ())
        cursor.execute(insert_query, (int(movie.movie_id), movie.movie_url, movie.name, movie.poster_url, movie.year,
                                      ','.join(movie.genres), movie.director, movie.imdb_rating, movie.description,
                                      movie.watch_url, movie.trailer_url, movie.dt, movie.count))
        con.commit()


def fetch_from_db(movie_id: str, username: str) -> MovieInfo | None:
    table_query = """
    SELECT name FROM sqlite_master WHERE name = ?
    """
    select_query = f"""
    SELECT * FROM {username} WHERE movie_id = ?
    """
    update_query = f"""
    UPDATE {username} SET dt = ?, count = count + 1 WHERE movie_id = ?
    """
    with con:
        cursor = con.cursor()
        cursor.execute(table_query, (username,))
        if cursor.fetchone() is None:
            return None
        cursor.execute(select_query, (movie_id,))
        if (fetched := cursor.fetchone()) is not None:
            _, name, url, poster_url, year, genres, director, imdb, description, watch, trailer, dt, count = fetched
            cursor.execute(update_query, (datetime.now(), movie_id))
            return MovieInfo(
                movie_id, name, url, poster_url, year, genres.split(','),
                director, imdb, description, watch, trailer, dt, count
            )
        else:
            return None


def get_username(message: types.Message) -> str:
    return "anonymous" if message.from_user is None or message.from_user.username is None \
        else message.from_user.username


async def run() -> None:
    bot_token = "unknown" if (get_token := os.getenv("CINEMABOT_TOKEN")) is None else get_token
    bot = Bot(token=bot_token, parse_mode=ParseMode.HTML)
    await dp.start_polling(bot)
    con.close()


if __name__ == '__main__':
    con = sqlite3.connect("cinemabot.db")
    asyncio.run(run())
