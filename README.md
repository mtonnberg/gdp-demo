A demo of how to enhance a webapi built with Servant by capturing knowledge in types witht the help of the "Ghosts of Departed proofs"-package (GDP)

# How to run
- install nix-shell
- run `nix-shell`
- run `stack run`

# Important files
- ApiDeclaration.hs
- ApiImplementation.hs

try

http://localhost:8484/habitats/north-pole/animals/5?user=user1
http://localhost:8484/habitats/north-pole/animals/5?user=user2
http://localhost:8484/habitats/savanna/animals/5?user=user1
http://localhost:8484/habitats/savanna/animals/5?user=user2