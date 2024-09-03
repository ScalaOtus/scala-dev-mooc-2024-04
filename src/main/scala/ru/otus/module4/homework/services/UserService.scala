package ru.otus.module4.homework.services

import io.circe._
import io.circe.generic.semiauto._
import ru.otus.module4.homework.dao.entity.{Role, RoleCode, User}
import ru.otus.module4.homework.dao.repository.UserRepository
import ru.otus.module4.phoneBook.db
import zio.Has
import zio.RIO
import zio.ZLayer
import zio.macros.accessible

@accessible
object UserService{
    type UserService = Has[Service]

    trait Service{
        def listUsers(): RIO[db.DataSource, List[User]]
        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
    }

    class Impl(userRepo: UserRepository.Service) extends Service{
        val dc = db.Ctx
        import dc._

        def listUsers(): RIO[db.DataSource, List[User]] =
        userRepo.list()


        def listUsersDTO(): RIO[db.DataSource,List[UserDTO]] = listUsers().map(
            _.map((userRecord:User)=>
                   for {
                       userRoles <- userRepo.userRoles(userRecord.typedId)
                   } yield (UserDTO(userRecord, userRoles.toSet ))
                 )
        )

        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] = {
         dc.transaction(
            for {
            userRecord<-userRepo.createUser(user)
            userRolesRecord<-userRepo.insertRoleToUser(roleCode, user.typedId)
            userRoles <- userRepo.userRoles(user.typedId)
           } yield(UserDTO(userRecord,userRoles.toSet))
         )
        }

        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource,List[UserDTO]] = {
            userRepo.listUsersWithRole(roleCode).map(
                (user: User) =>
                    for {
                        userRoles <- userRepo.userRoles(user.typedId)
                    } yield (UserDTO(user, userRoles.toSet))
            )
        }
        
        
    }

    val live: ZLayer[UserRepository.UserRepository, Nothing, UserService] = ZLayer.fromServices[UserRepository.Service, UserService.Service]((repo)=>new Impl(repo))
}

case class UserDTO(user: User, roles: Set[Role])

