all: bot.hs hurl.hs
	cabal configure && cabal build

#bot:bot.hs
#	ghc bot.hs

