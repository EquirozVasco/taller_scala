import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.concurrent.TimeUnit

class Libro(val titulo: String, val codigo_ISBN: String, val numero_paginas: Int, val editorial: String,
            val formato: String, val categoria: String, val autor: String, val ejemplar_id: String, var numero_edicion: Int)


class Usuario(val nombre_completo: String, val direccion: String, val cedula: String, val telefono: Int, var vetado: Int, var tipo: String, var prestado: Int){
}

class Profesor(cedula: String, nombre_completo: String, direccion: String, telefono: Int, vetado: Int = 0, tipo: String= "Profesor", prestado: Int = 0)
  extends Usuario(cedula, nombre_completo, direccion, telefono, vetado, tipo, prestado){
}

class Estudiante(cedula: String, nombre_completo: String, direccion: String, telefono: Int, vetado: Int = 0, tipo: String= "Estudiante", prestado: Int = 0)
  extends Usuario(cedula, nombre_completo, direccion, telefono, vetado, tipo, prestado){
}

class Prestamo(val cedula: String, val nombre_completo: String, val codigo_ISBN: String, val titulo: String, var fecha_prestamo: String, var fecha_entrega: String, var estado: Int)


def Prestar(arreglo_prestamos: List[Prestamo], arreglo_usuarios: List[Usuario], arreglo_libros: List[Libro],  cedula: String, codigo_ISBN: String, fecha: String): List[Prestamo] = {

  def fecha_entrega: String = {
    val fecha_inicial = new SimpleDateFormat("dd-MM-yyyy")
    val fecha_final = Calendar.getInstance

    fecha_final.setTime(fecha_inicial.parse(fecha))
    fecha_final.add(Calendar.DATE, 8)

    return fecha_inicial.format(fecha_final.getTime)
  }

  var contador_libros_prestados:  Int = 0

  arreglo_prestamos.foreach(user => {
    if(user.cedula == cedula){
      contador_libros_prestados += 1
    }
  })

  arreglo_libros.foreach(libro => {
    if(libro.codigo_ISBN == codigo_ISBN){
      arreglo_usuarios.foreach(usuario => {
        if(usuario.cedula == cedula){
          if(usuario.vetado == 0) {
            if(usuario.tipo == "Estudiante"){
              if(contador_libros_prestados < 5){
                val Prueba:List[Prestamo]= new Prestamo(cedula, usuario.nombre_completo, libro.codigo_ISBN, libro.titulo, fecha, fecha_entrega, 5) :: Nil
                return Prueba
              }
              else{
                println("El estudiante superó el límite de prestamos")
              }
            }
            else if(usuario.tipo == "Profesor"){
              val Prueba:List[Prestamo]= new Prestamo(cedula, usuario.nombre_completo, libro.codigo_ISBN, libro.titulo, fecha, fecha_entrega, 5) :: Nil
              return Prueba
            }
          } else {
            println("El usuario se encuentra vetado")
          }
        }else{
          println("Usuario no encontrado")
        }
      })
    }
  })
  return null
}

def verificar_vencimiento (arreglo_usuarios: List[Usuario], arreglo_prestamos: List[Prestamo]){

  def Diferecia(fecha_inicial: String, fecha_final: String): Int = {
    val sdf = new SimpleDateFormat("dd-MM-yyyy")
    val fecha_prestamo = sdf.parse(fecha_inicial)
    val fecha_devolucion = sdf.parse(fecha_final)

    val diff = fecha_prestamo.getTime - fecha_devolucion.getTime

    val time = TimeUnit.DAYS
    val diffrence = time.convert(diff, TimeUnit.MILLISECONDS)
    return diffrence.toInt
  }

  var contador_retraso: Int = 0
  var documento : String

  arreglo_prestamos.foreach(prestamo =>{

    if(Diferecia(prestamo.fecha_prestamo, prestamo.fecha_entrega) < 0){
      contador_retraso += 1
      documento = prestamo.cedula
    }
  })

  if(contador_retraso != 0){
    arreglo_usuarios.foreach(usuario =>{
      if (usuario.cedula == documento){
        println("El usuario con cédula: " + usuario.cedula + "Tiene un veto de: " + usuario.vetado)
        usuario.vetado += contador_retraso * 8
      }
    })
  }else{
    println("Sin vencimiento")
  }
}

class Buscar_libro_prestado(val arreglo_prestamos: List[Prestamo], val cedula: String){
  arreglo_prestamos.foreach(prestamo => {
    if(prestamo.cedula == cedula){
      println("El libro prestado es: " + prestamo.titulo)
    }
  })
}

class Buscar_libro_por_categoria(val arreglo_libros: List[Libro], val categoria: String){
  arreglo_libros.foreach(libro => {
    if(libro.categoria == categoria){
      println("El Libro: " + libro.titulo + " y el Codigo es: " + libro.codigo_ISBN + " Categoria: " + libro.categoria)
    }
  })
}

class Buscar_libro_por_formato(val arreglo_libros: List[Libro], val formato: String){
  arreglo_libros.foreach(libro => {
    if(libro.formato == formato){
      println("El Libro: " + libro.titulo + " y el Codigo es: " + libro.codigo_ISBN + " Formato: " + libro.formato)
    }
  })
}

class Buscar_libro_por_autor(val arreglo_libros: List[Libro], val autor: String){
  arreglo_libros.foreach(libro => {
    if(libro.autor == autor){
      println("El Libro: " + libro.titulo + " y el Codigo es: " + libro.codigo_ISBN + " Autor: " + libro.autor)
    }
  })
}

class BuscarLibroEditorial(val arreglo_libros: List[Libro], val editorial: String){
  arreglo_libros.foreach(libro => {
    if(libro.editorial == editorial){
      println("El Libro: " + libro.titulo + " y el Codigo es: " + libro.codigo_ISBN + " Editorial: " + libro.editorial)
    }
  })
}

object ScalaApp extends App {
  val usuario:List[Usuario]= new Estudiante("456", "Dave Grohl", "CA",telefono = 45679:Int)::
    new Estudiante("123", "Pastor López", "MED",telefono = 741258:Int)::
    new Estudiante("789", "Rafael Lechowski", "MAD",telefono = 951357:Int)::
    new Profesor("1852", "Celia Cruz", "CUB",telefono = 45669) ::Nil

  val libro:List[Libro] = new Libro("De aimales a dioses","123",320,"Debate","impreso","Ciencias Sociales","Noah Harari","543",4)::
    new Libro("1984","958-97628-9-1",252,"Skala","impreso","Novela","George Orwell","7553",2)::
    new Libro("Rebelión en la granja","978-958-8773-84-1",139,"Debolsillo","impreso","Novela","George Orwell","7553",1)::Nil

  var prestamo = List[Prestamo]()
  val A: List[Prestamo] = Prestar(prestamo, usuario, libro,"456", "123", "31-05-1989")
  val B: List[Prestamo] = Prestar(prestamo, usuario, libro,"123", "958-97628-9-1", "30-10-2021")
  val C: List[Prestamo] = Prestar(prestamo, usuario, libro,"1852", "978-958-8773-84-1", "21-10-1925")

  if( A != null){prestamo = prestamo ::: A}
  if( B != null){prestamo = prestamo ::: B}
  if( C != null){prestamo = prestamo ::: C}


  println(verificar_vencimiento(usuario, prestamo))

  println("Buscar Libros Prestados de Persona")
  println(Buscar_libro_prestado(prestamo, "123"))

  println("Buscar por Categoria")
  println(Buscar_libro_por_categoria(libro, "Novela"))

  println("Buscar por Formato")
  println(Buscar_libro_por_formato(libro, "impreso"))

  println("Buscar por Autor")
  println(Buscar_libro_por_autor(libro, "George Orwell"))

  println("Buscar por Editorial")
  println(BuscarLibroEditorial(libro, "Debate"))

}
