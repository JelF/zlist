# Data.Zlist
Data.Zlist is designed for better output in science problems  
This is so alpha, that you can't get it by cabal

## Usage
E.g. you have a quest to find the lowest value of function `f`,
which could be taken on known set (which could be represented by list).
You basically want to know, for which elements it is reached.

```haskell
solve f range = print zmaximum (compare) $ f <$> zlist range
```

If you need to get a minimum value, you can use  

```haskell
solve f range = print zmaximum (flip compare) $ f <$> zlist range
```

## Contributing
Not needed. If you really want to make a PR, better create an issue.
I generally use it for learning how to Haskell.  
However, [license](LICENSE) allows you to fork or any other usage.
