--1º
type Hora = (Int,Int)
type Etapa = (Hora,Hora)
type Viagem = [Etapa]
--Funçoes da ficha 1
--3ºa
horas' :: Hora -> Bool
horas' (h,m) = (h >= 0) && (h < 24) && (m >= 0) && (m < 60)

--3ºb
compHoras :: Hora -> Hora -> Bool
compHoras (a,b) (x,y) = ((a < x) || ( a==x && b < y))

--3ºc
convHoras :: Hora -> Int
convHoras (x,y) = (x*60) + y

--3ªd
convHoras' :: Int -> Hora
convHoras' x = (div x 60, mod x 60)

--3ºe
difHoras :: Hora -> Hora -> Int
difHoras a b = if (compHoras a b) then (convHoras b) - (convHoras a) else (convHoras a) -(convHoras b)

--3ºf
adicionaMinutos :: Hora -> Int -> Hora
adicionaMinutos hora mins = convHoras' ((convHoras hora) + mins)

--Ficha 3
--1ªa
etapaValida :: Etapa -> Bool
etapaValida (h,t) = (horas' h) && (horas' t) && (compHoras h t)

--1ªb
viagemValida :: Viagem -> Bool
viagemValida [] = True
viagemValida [x] = etapaValida x
viagemValida ((x,xs):(y,ys):ts) = if ((etapaValida (x,xs)) && (compHoras xs y))  then viagemValida ((y,ys):ts)  else False

--1ºc
--calacTotal :: Viagem -> Hora
--calacTotal l = convHoras'(aux l)
--	 where aux ((h1,h2):es) = convHoras (difHoras h1 h2) + aux es
--	       aux [] = 0