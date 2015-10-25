# Data Format

This is not official documentation on the data format used often in Paradox
Interactive games. There is no specification. What I document here is what I
have seen in the wild. I have seen the source code for an old version of the
game and there is manual serialization and deserialization, so there are no
guarantees.

Here is an example document of all the crazy things you can see. You'll notice
that the format resembles JSON.

The last time I checked, the files are encoded as Windows 1252.

    [lang=text]
    # This is a comment
    # The root element is always an object that map string keys to another object:
    # (string, integer, float, date, array, object)

    # Strings
    foo=bar

    # Strings with quotes that can span multiple lines
    baz=" hello  ##
          cheese"

    # Dates (YYYY.M.D)
    start=1841.2.3

    # Dates with hour
    middle=1841.2.3.4

    # Dates that are quoted
    end="1300.10.1"

    # Integers
    type=49

    # Floating point (always in the format #.### or #.#####)
    strength=10.435
    morale=5.10875

    # An array is always composed of homogenous types
    nums={1 2 3 4}

    # Keys don't have to be unique
    core=YOU
    core=MEE

    # Sub object
    army={
        # Multiple keys per object are allowed, notice that objects that share the
        # same keys don't have to have the exact same properties!
        unit={
            name="1st unit"
        }

        unit={
            name="1st unit"

            # Boolean (yes -> true, no -> false)
            patrol=yes
        }

        # Empty object (bug with the format?)
        { }

        # An array of objects without keys
        attachments={
            {
                id=34
            }
            {
                id=55
            }
        }
    }


I recommend parsing the objects as an array of tuples of strings and objects
because the order of the properties may matter (a map or multi-map doesn't
necessarily guarantee element order). This also helps with writing the data
structure back out.
