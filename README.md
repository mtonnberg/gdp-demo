# Demo of Servant-GDP  
A demo of how to enhance a webapi built with [Servant](https://haskell-servant.github.io/) (A web api framework) by capturing knowledge in types with the help of the "Ghosts of Departed proofs"-package (GDP).

This is a way to embrace [knowledge-as-code](https://carboncloud.com/2020/12/07/tech-knowledge-as-code/) for fun and profit.

### How to run
- install nix-shell
- run `nix-shell`
- run `stack run`

### Important files
- ApiDeclaration.hs
- ApiImplementation.hs
- Animals.hs
- DomainProofs.hs

### Urls to try

http://localhost:8484/habitats/north-pole/animals/5?user=user1
http://localhost:8484/habitats/north-pole/animals/5?user=user2
http://localhost:8484/habitats/savanna/animals/5?user=user1
http://localhost:8484/habitats/savanna/animals/5?user=user2