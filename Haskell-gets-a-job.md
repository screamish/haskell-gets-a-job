slidenumbers: true
build-lists: true
autoscale: true

# [fit] Haskell gets a job

## [fit] helping everyone get a job!

-----------------------------------------------
# Haskell at SEEK

![]( https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/Haskell-Logo.svg/2000px-Haskell-Logo.svg.png )

-----------------------------------------------
# Simon Fenton

- SEEK
- Search stream
- Developer / Tech lead
- Build back-end batch and streaming services

### @screamish

-----------------------------------------------
# Haskell at SEEK

1. FP + me
1. FP + SEEK
1. Haskell + my team
1. Haskell + SEEK

^ Slides will be put online in markdown and PDF (probably).
Footnotes/links wherever possible.

-----------------------------------------------
# FP + me

-----------------------------------------------
# FP + me

- Unimelb CompSci (2000)
  - Haskell
  - Prolog

^ Haskell intrigued but puzzled me at uni.
Wanted to make things for the web.
Smitten with PHP and Java

-----------------------------------------------
> Haskell is not a difficult language to use.
> Haskell is difficult to teach effectively.
-- Haskell Book

^ I'll come back to this book later.
This resonated with me in 2015 when I read it.
I'd add: Haskell forces you to confront the true complexity
of your programs. You can't hand wave stuff away (well, you can, but it's opt-in).
Why didn't Haskell click for me at all in 2000?

-----------------------------------------------
# 'Enterprise Software'

- a.k.a. *The OOP Tar-pit*
- NullReferenceException
- Shared mutable state
- So many bugs AND so much time testing (manual and automated)

^ Very little I learned seemed to have lasting value.
Loads of effort, very little change in the nature and quantity of problems, particularly w.r.t scale

-----------------------------------------------
# People show me another way

- F# community in London
- (small) successes with F# at JustGiving
- Tools to move TB of user content into new content service

^ Community!
Don Syme, Tomas Petricek, Phil Trelford (also this guy SPJ?).
12 years after first trying Haskell, things started making sense.

-----------------------------------------------
# FP + me

- Immutable
- Terse
- Types

^ Algebraic data types - sum types.
Model the problem in many more flexible ways.

-----------------------------------------------
# FP + SEEK

-----------------------------------------------
# FP + SEEK (2014)

- Some F# in production
- Gain adoption via tooling and tests
- [Programming Languages - Coursera](https://www.coursera.org/course/proglang)

^ Joined SEEK in 2014, some F# in prod, but not my team.
Mostly C# .NET on Windows.
Started with tooling again; FAKE, Paket.
Many curious devs & QAs asking me great questions, felt I couldn't answer well enough so ProgLang course.
Great comparison of different PLs and their affordances.
Expression Problem.

-----------------------------------------------
# FP + SEEK (2015)

- Speed to market / cheap(er) experiments
- Microservices & Devops
- *'Cambrian explosion'* of PL
- F# - Scala - Clojure - Haskell - Go - Javascript (Node)
- Internal [F# Workshop](https://github.com/SEEK-Jobs/fsharp-workshop)
- ... Docker?

^ Delivery and Product push for speed to market.
The landscape started to change, rapidly.
3 whole-day workshops focusing on FP via F#; devs, testers, BAs and more.

-----------------------------------------------
# Haskell + my team

^ From the team:
Too hardcore – absolutely no mutation, ever?
Weird, lots of crazy meaningless symbols
Requires knowledge of category theory to use
Only used in academia
Are there even libraries for things?

-----------------------------------------------
# Haskell + my team

- Sept 2015 - No[^1] Haskell experience
- Lunchtime group starts [NICTA FP Course](https://github.com/NICTA/course)
- Greenfield F# on Mono :confused:
- Brownfield F# on Mono[^2] :sob:

^ Andrew Browne starts running lunchtime NICTA course, >10 devs join including my whole team.
Many seem to enjoy it, plenty find it hard, I fell off pretty quickly.
Blindly throwing code at GHC until it compiled. Didn't feel like I was learning much.
Greenfield F# on Mono 'works' but was painful. Not all forms of TCO are implemented.
Brownfield F# on Mono unworkable.
Why pursue it?

-----------------------------------------------
# Constraints & Context

- AWS
- 12-factor App[^3]
- Docker

^ Team really enjoyed linux on AWS.
Enabled us to tap into a much richer world of software and tools.
Time to make some hard choices.

-----------------------------------------------
# Reckoning

- Stay with F# and go back to Windows?
- Internal Hackathon right around the corner
- Clojure?
- Scala?
- Haskell!

^ Staying with F# -> stay with Windows.
Not easy on AWS, relatively small community automating Windows and making it simple.
Hackathon means 3 days to test ourselves and our ideas.
I'd been getting pretty excited with Paredit in Spacemacs trying out Clojure.
Thought it looked like the safe choice.
Very pleasantly surprised the team chooses Haskell with zero prompting from me.

-----------------------------------------------
# A plan for Haskell

- What does success look like?
- Vertical slice of current F# app
- AWS SDKs critical (S3, DynamoDB)
- IO (CSV files, HTTP, CLI args)
- Tests (correctness measures)
- Docker deploy a la 12-factor App

^ Fairly confident we could learn enough, quickly, to create useful software out of existing libraries.
If libraries didn't exist for what we needed, that would be a concern.
Success = deliver thin, vertical slice of current app's behaviour and have reasonable confidence in ability to port the rest in a reasonable time.

-----------------------------------------------
# Divide & Conquer

- 2 on core domain
- 1 on input CSV from S3
- 1 on output to HTTP

^ Some team members were already committed to other hack teams, so only 4 of us.

-----------------------------------------------
# The pleasures

- Stack

-----------------------------------------------
# Seriously, Stack is amazing!

-----------------------------------------------
# Stack[^5]

Fresh dev laptop

```
> brew install haskell-stack
> git clone http://the-thing.git
> stack setup
> stack test
```

^ Getting new team members up and running with our Haskell apps has been a delight.
Except if they had previous installs of GHC from The Haskell Platform or homebrew etc.

-----------------------------------------------
# Cross-platform

- Windows and OS X dev envs
- Linux in Docker for production
- Holy moly those Docker build times
- Docker build-caching to the rescue

^ Windows and OS X, a few niggles but otherwise easy to get going
and easy to keep going.
Docker builds were >45 minutes. WAT.
Luckily ridiculous node.js build times had led other teams at SEEK
to already solve this problem.

-----------------------------------------------
# Amazonka (AWS)

```haskell
downloadFile :: Region     -- ^ Region to operate in.
             -> BucketName
             -> ObjectKey  -- ^ The source object key.
             -> FilePath   -- ^ The destination file to save as.
             -> IO ()
downloadFile r b k f = do
    lgr <- newLogger Debug stdout
    env <- newEnv r Discover <&> envLogger .~ lgr

    runResourceT . runAWST env $ do
        rs <- send (getObject b k)
        view gorsBody rs `sinkBody` CB.sinkFile f
```

^ This code was written in less than a day by someone with
very little prior Haskell experience.
Definitely intimidating at first look. Conduit? Lens?
But following some samples and hammering at the types we
got things working pretty quickly.

-----------------------------------------------
# Wreq (HTTP)

- Awesome doco! [Wreq Tutorial](http://www.serpentine.com/wreq/tutorial.html)
- HTTP "hello world" was super quick
- Internal-only, legacy APIs with 'interesting' auth protocols
- Can we handle going off-road?

-----------------------------------------------
# Wreq - JSON parsing

```haskell
instance FromJSON Response where
  parseJSON (Object o) =
          Response <$> o .: "status"
                   <*> o .: "nextHref"
                   <*> o .: "id"
  parseJSON _ = mzero
```

^ Applicative shines!
What's this mzero? MonadPlus? Oh boy...
Alright, for now I'll just accept it gives me a way to say "failed parsing"

-----------------------------------------------
# Wreq brought a friend

```haskell
import Control.Lens hiding ((.=))
import Data.Aeson.Lens

allIds = values . key "id" . _Integer
```

^ I wonder what the type of that is?

-----------------------------------------------
# Lens

```haskell
allIds :: (Integer -> Const (Endo [Integer]) Integer)
       -> LB.ByteString
       -> Const (Endo [Integer]) LB.ByteString
allIds = values . key "id" . _Integer
```

^ Oh boy.

-----------------------------------------------
# Lens

```haskell
allIds :: (Integer -> Const (Endo [Integer]) Integer)
       -> LB.ByteString
       -> Const (Endo [Integer]) LB.ByteString

allIds :: Traversal' LB.ByteString Integer
allIds = values . key "id" . _Integer
```

^ Reaching this level of understanding with Lens took a good amount of time.
But it has been fun.

-----------------------------------------------
# The pains

- Regex[^4]
- String vs Text vs ByteString
- Text formatting (Text.Printf vs text-format vs ???)

^ Woah that's a lot of reading/learning.
A theme with Haskell libs/docs, lots of information but no nice on-ramp for the beginner.
We've settled on regex-pcre-builtin for now but it was a lot to learn.
Strings/Text still confuse us regularly.

-----------------------------------------------
# Hackathon retro

- No major concerns
- Loving it!
- Let's plot a course for total cut-over

-----------------------------------------------
# [fit] Full-time Haskell

^ When you're doing things all the time, your learning just flies.

-----------------------------------------------
# HTTP testing

- Oh, easy, composable DSLs with our Free Monads!!
- Freer monads, etc...
- Stop. We really don't get this.
- What do we do?

-----------------------------------------------
# Isolating HTTP

```haskell
class Monad m => MonadHttp c m | m -> c where
  get :: Url -> Options -> m ByteString
  getSigned :: Url -> SignedOptions -> m ByteString
  getJson :: (MonadThrow m, FromJSON a) => Url -> Options -> m a
  sign :: Url -> Options -> m SignedOptions

newtype HttpT c m a = HttpT (ReaderT c m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance (HttpContext c, MonadIO m) => MonadHttp c (HttpT c m) where

newtype MockHttpT c m a =
    MockHttpT (ReaderT (MockHttpContext c m)
              (WriterT [RecordedRequest] m) a)
  deriving (Functor, Applicative, Monad, MonadIO,
            MonadThrow, MonadCatch)
```

^ This gave us a way to test our HTTP code in a way we could still
reason about the composition back into our main DSL.

-----------------------------------------------
# HTTP - PACT testing

^ Some Node.js and Ruby pragmatism.
We're working on a Haskell wrapper for the ruby PACT mock service.

-----------------------------------------------
# Free Monad bites back

- The 'naive' Free Monad performs terribly
- Saved by the Church encoded Free Monad

^ Naive free monad performed horribly.
We freak out.
We reach out.
We learn about Church Encoding.
We rejoice!

-----------------------------------------------
# Performance

- Mostly amazing
- But hard for us (still) to reason about
- Space leaks :disappointed:

^ Eager to start using Pipes and/or Conduit in the large in our programs.
But finding it hard to adapt the outer shell.

-----------------------------------------------
# Some nice surprises

- Haskell for scripting
- *#!* thanks to stack
- Most scripts promoted to apps sharing a lib
- Much fewer tests (property tests > unit tests)
- Really easy to drop into an unfamiliar part of codebase
- No bugs! (so far)

^ We were using python for most infra coding.
Some team members had a few ad-hoc analysis tasks that could
have been done with postman/curl and grep and csvkit, but realised
you'd be reimplementing a lot of the batch tasks' domain.

-----------------------------------------------
# Haskell takes over

- Launched the Haskell v2 in dry-run, side-by-side with F# v1 in March
- v2 and v1 change places in April
- Shutdown v1 in May
- No surprises, smooth sailing all the way

^ From the team:
“If it compiles, it works” – true surprisingly often!
Space leaks suck so bad – very hard to reason about memory usage, and to a lesser extent, execution time
Compiler-enforced pure code – I can trust code does what it’s types says it does. I don’t need to dive into it as much to make sure it’s not doing Bad Things™
“Global state” is explicit.
Tooling is acceptable at best (Atom, ghc-mod). Really miss ReSharper.
The big joke with “what is a Monad”, I feel is like the Matrix. You can’t be told what it is, you just have to experience it for yourself - For me, they properly clicked when I stopped trying to learn what they were, and just start using them and figure out the commonality. This seems to hold for other principles too, as trying to figure out these abstract concepts is difficult until you start using concrete examples.

-----------------------------------------------
# Haskell + SEEK

^ So where do we go from here with Haskell and the wider SEEK dev community?

-----------------------------------------------
# Haskell 2016?

^ It would be really awesome to have an intro to Haskell for people starting in 2016.
Explaining the history, and which parts can be ignored, which parts are being deprecated.

-----------------------------------------------
# Then

```
default-extensions:
    OverloadedStrings
```

# Now

```
default-extensions:
    OverloadedStrings
  , TupleSections
  , FlexibleContexts
  , RecordWildCards
  , ScopedTypeVariables
```

^ Some others probably not far behind, LambdaCase,
BangPatterns, MultiWayIf, etc...

-----------------------------------------------
# Haskell resources we like

- [Haskell is easy](http://haskelliseasy.com)
- [Stackage](http://stackage.org)
- [24 Days of Hackage on conscientiousprogrammer.com](http://conscientiousprogrammer.com/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/)
- [Lens over tea](https://artyom.me/lens-over-tea-1)
- [Haskell for all](http://www.haskellforall.com/)

-----------------------------------------------
## HaskellBook.com

### Haskell Programming from first principles

^ December 2015 bought myself a copy.
Soon after bought copies for the whole team.

-----------------------------------------------
> Haskell is not a difficult language to use.
> Haskell is difficult to teach effectively.
-- Haskell Book

-----------------------------------------------
## Spaced repetition and iterative deepening

^ Profound difference between learning Haskell and most other proglangs.
Haskell knowledge can't be rushed, but each new piece builds more satisfyingly on previous knowledge than most other langs.

-----------------------------------------------
# Haskell + SEEK

- It's in production, doing serious business
- Interest growing
- YOW! Lambda Jam 2016
- Compose (Melbourne)

^ Biggest SEEK contingent to LambdaJam yet.

-----------------------------------------------
# [fit] Thank you


[^1]: Besides my uni experience and light dabbling in between

[^2]: [What's up with mono?](https://github.com/SEEK-Jobs/whatsupwithmono/issues)

[^3]: [The Twelve-Factor App](http://12factor.net/)

[^4]: [Regular Expressions @ Haskell Wiki](https://wiki.haskell.org/Regular_expressions)

[^5]: [Haskell Stack](www.haskellstack.org)
