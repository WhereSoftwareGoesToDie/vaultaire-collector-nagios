-- | Hashing as well as encoding/decoding caches

module Cache where

import Crypto.MAC.SipHash
import Data.Bifunctor (bimap)
import qualified Data.Binary as B
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.Serialize as Serialize
import Data.Set
import Data.Text
import qualified Data.Text as T(unpack)
import Data.Word

type SourceDictCache = Set Word64

emptyCache :: SourceDictCache
emptyCache = fromList []

decodeCache :: L.ByteString -> Either String SourceDictCache
decodeCache rawData =
    let result = G.runGetOrFail B.get rawData in
    case result of
        Left (_, _, e)         -> Left e
        Right (_, _, assocList) -> Right (fromList assocList)

encodeCache :: SourceDictCache -> L.ByteString
encodeCache cache = B.encode $ toList cache

-- | Hashes the association list of a SourceDict using SipHash
-- Hashes are used to ignore queuing redundant SourceDict update
hashList :: [(Text, Text)] -> Word64
hashList list =
    let unpackedList = Prelude.map (bimap T.unpack T.unpack) list in
    let (SipHash ret) = hash (SipKey 0 0) (Serialize.encode unpackedList) in
    ret
