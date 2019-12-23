module Day23 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe

import Control.Monad

import IntcodeStep
import Lib

{-
--- Day 23: Category Six ---

The droids have finished repairing as much of the ship as they can. Their report indicates that this was a
Category 6 disaster - not because it was that bad, but because it destroyed the stockpile of Category 6 network
cables as well as most of the ship's network infrastructure.

You'll need to rebuild the network from scratch.

The computers on the network are standard Intcode computers that communicate by sending packets to each other.
There are 50 of them in total, each running a copy of the same Network Interface Controller (NIC) software (your
puzzle input). The computers have network addresses 0 through 49; when each computer boots up, it will request
its network address via a single input instruction. Be sure to give each computer a unique network address.

Once a computer has received its network address, it will begin doing work and communicating over the network
by sending and receiving packets. All packets contain two values named X and Y. Packets sent to a computer are
queued by the recipient and read in the order they are received.

To send a packet to another computer, the NIC will use three output instructions that provide the destination
address of the packet followed by its X and Y values. For example, three output instructions that provide the
values 10, 20, 30 would send a packet with X=20 and Y=30 to the computer with address 10.

To receive a packet from another computer, the NIC will use an input instruction. If the incoming packet queue
is empty, provide -1. Otherwise, provide the X value of the next packet; the computer will then use a second
input instruction to receive the Y value for the same packet. Once both values of the packet are read in this
way, the packet is removed from the queue.

Note that these input and output instructions never block. Specifically, output instructions do not wait for
the sent packet to be received - the computer might send multiple packets before receiving any. Similarly,
input instructions do not wait for a packet to arrive - if no packet is waiting, input instructions should
receive -1.

Boot up all 50 computers and attach them to your network. What is the Y value of the first packet sent to
address 255?
-}

data Packet = P Integer Integer Integer deriving (Show)
addr (P a _ _) = a
x (P _ x _) = x
y (P _ y _) = y

day23 :: [String] -> IO Integer
day23 ls = do
  let prog = parse $ head ls
      pending = Map.fromList [(i, (prog, 0, [i])) | i <- [0..49]]
  loop pending

  where
    loop :: Map.Map Integer (Prog, Addr, [Integer]) -> IO Integer
    loop computers = do
      let pending' = Map.map (\(prog, pc, inputs) ->
                               let (prog', pc', outputs, _) = run0 pc prog inputs []
                               in (prog', pc', outputToPackets outputs)
                               ) computers   :: Map.Map Integer (Prog, Addr, [Packet])
      
      forM_ (Map.toList pending') $ \(a, (_, _, ps)) -> do
          putStrLn $ (show a) ++ " -> " ++ (show ps)                        
                               
          {- Accumulate outgoing packets by destination -}
      let outgoing = Map.foldl (\acc (_, _, outputs) ->
                                  foldl (\acc p -> Map.insertWith (++) (addr p) [p] acc)
                                        acc outputs
                                  ) Map.empty pending'

          {- stop ? -}
          killer = Map.lookup 255 outgoing
              
          {- assign packets -}
          nextIter = Map.mapWithKey (\a (prog', pc', _) ->
                                      case Map.lookup a outgoing of
                                        Nothing -> (prog', pc', [-1])
                                        Just ps -> (prog', pc', packetsToInputs ps)
                                     ) pending'
      
      if isJust killer
      then let ((P _ x y):ps) = fromJust killer in return y
      else loop nextIter
    
    outputToPackets o = map (\[a,x,y] -> P a x y) (chunksOf 3 o)
    packetsToInputs ps = map (\(P _ x y) -> [x, y]) ps & concat


{-
Packets sent to address 255 are handled by a device called a NAT (Not Always Transmitting). The NAT is
responsible for managing power consumption of the network by blocking certain packets and watching for idle
periods in the computers.

If a packet would be sent to address 255, the NAT receives it instead. The NAT remembers only the last packet
it receives; that is, the data in each packet it receives overwrites the NAT's packet memory with the new
packet's X and Y values.

The NAT also monitors all computers on the network. If all computers have empty incoming packet queues and are
continuously trying to receive packets without sending packets, the network is considered idle.

Once the network is idle, the NAT sends only the last packet it received to address 0; this will cause the
computers on the network to resume activity. In this way, the NAT can throttle power consumption of the network
when the ship needs power in other areas.

Monitor packets released to the computer at address 0 by the NAT. What is the first Y value delivered by the
NAT to the computer at address 0 twice in a row?
-}

day23b :: [String] -> IO [Packet]
day23b ls = do
  let prog = parse $ head ls
      pending = Map.fromList [(i, (prog, 0, [i])) | i <- [0..49]]
  loop pending Nothing [P (-1) (-1) (-1), P (-2) (-2) (-2)]

  where
    loop :: Map.Map Integer (Prog, Addr, [Integer]) -> Maybe Packet -> [Packet] -> IO [Packet]
    loop computers nat natSent = do
      let pending' = Map.map (\(prog, pc, inputs) ->
                               let (prog', pc', outputs, _) = run0 pc prog inputs []
                               in (prog', pc', outputToPackets outputs)
                               ) computers   :: Map.Map Integer (Prog, Addr, [Packet])
      
      forM_ (Map.toList pending') $ \(a, (_, _, ps)) -> do
          putStrLn $ (show a) ++ " -> " ++ (show ps)                        
                               
          {- Accumulate outgoing packets by destination -}
      let outgoing = Map.foldl (\acc (_, _, outputs) ->
                                  foldl (\acc p -> Map.insertWith (\n o -> o ++ n) (addr p) [p] acc)
                                        acc outputs
                                  ) Map.empty pending'

      putStrLn $ "Sent to NAT: " ++ (show $ Map.lookup 255 outgoing)
          {- Handle NAT registration ? -}
      let nat' = case Map.lookup 255 outgoing of
                   Nothing -> nat
                   Just ps -> Just (last ps)
      putStrLn $ "NAT assigned = " ++ show nat'
              
          {- assign packets -}
      let (nextIter, natSent') =
            let plannedSend = Map.mapWithKey (\a (prog', pc', _) ->
                                                case Map.lookup a outgoing of
                                                  Nothing -> (prog', pc', [-1])
                                                  Just ps -> (prog', pc', packetsToInputs ps)
                                               ) pending'
            in  if Map.null outgoing && isJust nat'
                then
                  (Map.insert 0 (let Just (prog', pc', [-1]) = Map.lookup 0 plannedSend
                                     Just (P _ x y) = nat'
                                 in  (prog', pc', [x, y])) plannedSend, (fromJust nat') : natSent)
                else (plannedSend, natSent)           
      
      let ((P _ x0 y0):(P _ x1 y1):_) = natSent'
      if y0 == y1
      then return natSent'
      else loop nextIter nat' natSent'
    
    outputToPackets o = map (\[a,x,y] -> P a x y) (chunksOf 3 o)
    packetsToInputs ps = map (\(P _ x y) -> [x, y]) ps & concat

