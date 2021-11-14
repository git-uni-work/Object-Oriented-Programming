package sflix

/**
 * Movie metadata from the Movie database.
 *
 * @param id       movie unique identifier
 * @param title    movie title
 * @param category movie category which can be one of:
 *   - `1` - normal movie
 *   - `2` - new releases
 *   - `3` - movies for kids
 **/

class Movie(val id: Int, val title: String, val category: Int)
